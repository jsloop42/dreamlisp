//
//  DLPersistenceService.m
//  DreamLisp
//
//  Created by jsloop on 23/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLPersistenceService.h"

static DLPersistenceService *_dbService;
static NSString *_projDataDir;

@interface DLPersistenceService ()
@end

@implementation DLPersistenceService {
    NSManagedObjectContext *_prefixMOC;
    DLFileOps *_fops;
    NSFileManager *_fm;
    BOOL _isPrefixStoreInitialized;
}

@synthesize prefixMOC = _prefixMOC;
@synthesize prefixStoreCoordinator;
@synthesize prefixStore;
@synthesize prefixContainer;
@synthesize isPrefixStoreInitialized = _isPrefixStoreInitialized;

+ (instancetype)shared {
    @synchronized (self) {
        if (!_dbService) {
            _dbService = [DLPersistenceService new];
        }
        return _dbService;
    }
}

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (void)bootstrap {
    _fops = [DLFileOps new];
    _fm = [NSFileManager defaultManager];
    _projDataDir = @"Data";
    _isPrefixStoreInitialized = NO;
}

- (BOOL)checkIfPrefixStoreExists {
    NSURL *url = [self prefixStoreURL];
    return [_fm fileExistsAtPath:[url path]];
}

- (BOOL)deletePrefixStore {
    NSError *err;
    NSURL *prefixStoreURL = [self prefixStoreURL];
    if (self.prefixStoreCoordinator) {
        [self.prefixStoreCoordinator removePersistentStore:self.prefixStore error:&err];
        if (err) [DLLog errorWithFormat:@"Error closing prefix store: %@", err];
        [self.prefixStoreCoordinator destroyPersistentStoreAtURL:prefixStoreURL withType:NSSQLiteStoreType options:nil error:&err];
        if (err) [DLLog errorWithFormat:@"Error removing prefix store: %@", err];
    }
    if ([self checkIfPrefixStoreExists]) {
        NSURL *prefixStoreDir = [prefixStoreURL URLByDeletingLastPathComponent];
        NSURL *walFile = [prefixStoreDir URLByAppendingPathComponent:[[NSString alloc] initWithFormat:@"%@.sqlite-wal", DLConst.prefixStoreName]];
        NSURL *shmFile = [prefixStoreDir URLByAppendingPathComponent:[[NSString alloc] initWithFormat:@"%@.sqlite-shm", DLConst.prefixStoreName]];
        [_fm removeItemAtURL:prefixStoreURL error:&err];
        [_fm removeItemAtURL:walFile error:&err];
        [_fm removeItemAtURL:shmFile error:&err];
    }
    _isPrefixStoreInitialized = NO;
    return err == nil;
}

/*!
Checks if prefix store exists, else copies the pre-build store from the bundle to the app support folder. Invokes the given callback once done with the status.
*/
- (void)initPersistence:(void(^)(BOOL))callback {
    BOOL isPrefixStoreExists = [self checkIfPrefixStoreExists];
    if (!isPrefixStoreExists) {
        BOOL ret = [_fops copyFile:[self prefixStoreBundleURL] toURL:[self prefixStoreURL]];
        if (!ret) {
            [DLLog error:@"Error copying prefix store from bundle"];
        }
    }
    [DLLog debug:@"in initPersistence method"];
    [self initPrefixStore:^{
        [DLLog debug:@"initPrefixStore callback"];
        NSAssert(self.prefixStore, @"Prefix store should exist");
        NSAssert(self.prefixStoreCoordinator, @"Prefix store coordinator should exist");
        [self updateStateWithPrefix:^(BOOL status) {
            [DLLog debug:@"updateStateWithPrefix callback"];
            callback(status);
        }];
    }];
}

/*!
 Returns the prefix sqlite store bundle URL.
 */
- (NSURL *)prefixStoreBundleURL {
    NSBundle *bundle = [NSBundle bundleForClass:[self class]];
    NSString *prefixStorePath = [bundle pathForResource:DLConst.prefixStoreName ofType:@"sqlite"];
    return [[NSURL alloc] initFileURLWithPath:prefixStorePath];
}

/*!
 Generates the prefix store
 */
- (void)initPrefixStore:(void (^)(void))callback {
    /*
     1. Load the model from the bundle
     2. Initialize a persistence store from the model
     3. Save the store to the app's document directory
     // ..
         3.1. Check if the prefix store exists in the document directory
            If not, copy the DLPrefix.sqlite to the document directory and open
     4. Populate the store with the data loaded from plist. Just add the prefixes as is.
     5. Save the moc
     6. Copy the store to the project data dir
    */
    [DLLog debug:@"in initPrefixStore method"];
    NSURL *prefixMOMD = [[NSBundle bundleForClass:[self class]] URLForResource:DLConst.prefixStoreName withExtension:@"momd"];
    if (!prefixMOMD) {
        [DLLog error:@"Prefix MOMD file not found"];
        callback();
        return;
    }
    NSManagedObjectModel *mom = [[NSManagedObjectModel alloc] initWithContentsOfURL:prefixMOMD];
    if (!mom) {
        [DLLog error:@"Managed Object Model initialization failed"];
        callback();
        return;
    }
    self.prefixContainer = [[NSPersistentContainer alloc] initWithName:DLConst.prefixStoreName managedObjectModel:mom];
    DLPersistenceService __weak *weakSelf = self;
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_BACKGROUND, 0), ^{
        DLPersistenceService *this = weakSelf;
        NSError *err;
        this.prefixStore = [this.prefixContainer.persistentStoreCoordinator addPersistentStoreWithType:NSSQLiteStoreType configuration:nil URL:[self prefixStoreURL]
                                                                                               options:nil error:&err];
        if (!this.prefixStore) {
            [DLLog errorWithFormat:@"Failed to initialize prefix store: %@", err.description];
            if (callback) dispatch_async(dispatch_get_main_queue(), ^{ callback(); });
            return;
        }
        [[self.prefixContainer viewContext] setAutomaticallyMergesChangesFromParent:YES];
        this->_prefixMOC = [[NSManagedObjectContext alloc] initWithConcurrencyType:NSPrivateQueueConcurrencyType];
        this.prefixStoreCoordinator = self.prefixContainer.persistentStoreCoordinator;
        [this.prefixMOC setPersistentStoreCoordinator:this.prefixStoreCoordinator];
        this->_isPrefixStoreInitialized = YES;
        if (!callback) return;
        dispatch_async(dispatch_get_main_queue(), ^{
            [DLLog debug:@"Prefix store initialized"];
            callback();
        });
    });
}

/*!
 Returns the prefix store URL under app support folder. If the path to the store does not exist, it's created.
 */
- (NSURL *)prefixStoreURL {
    NSURL *docURL = [_fops applicationSupportDirectory];
    NSBundle *bundle = [NSBundle bundleForClass:[self class]];
    NSDictionary *info = [bundle infoDictionary];
    NSString *bundleID = [info objectForKey:@"CFBundleIdentifier"];
    NSURL *storeDirURL = [docURL URLByAppendingPathComponent: bundleID];
    NSString *storeDirPath = [storeDirURL path];
    if (![_fops isDirectoryExists:storeDirPath]) {
        [_fops createDirectoryWithIntermediate:storeDirPath];
    }
    return [storeDirURL URLByAppendingPathComponent:[[NSString alloc] initWithFormat:@"%@.sqlite", DLConst.prefixStoreName]];
}

/*!
 Inserts prefixes loaded from the plist into the core data store.
 */
- (void)insertPrefixToStoreInBatch:(void(^)(BOOL))callback {
    [self.prefixContainer performBackgroundTask:^(NSManagedObjectContext *bgMOC) {
        NSArray *prefixList = [self loadPrefixFromPList];
        NSEnumerator *iter = [prefixList objectEnumerator];
        NSString *elem = nil;
        BOOL status = YES;
        while ((elem = [iter nextObject]) != nil) {
            @autoreleasepool {
                DLPrefix *prefix = [NSEntityDescription insertNewObjectForEntityForName:@"DLPrefix" inManagedObjectContext:bgMOC];
                [prefix setName:elem];
            }
        }
        NSError *err;
        [bgMOC save:&err];
        if (err) {
            [DLLog errorWithFormat:@"Error persisting prefix context: %@", err];
            status = NO;
        }
        callback(status);
    }];
}

/*!
 Returns the prefix store project data URL
 */
- (NSURL *)prefixStoreProjectDataURL {
    NSMutableString *destStr = [NSMutableString new];
    [destStr appendString:[_fops projectRoot]];
    [destStr appendFormat:@"/%@/", _projDataDir];
    [destStr appendString:DLConst.prefixStoreName];
    [destStr appendString:@".sqlite"];
    return [[NSURL alloc] initFileURLWithPath:destStr];
}

/*!
 Copies the prefix sqlite store from the Application Support directory to the data folder of the project.
 */
- (BOOL)copyPrefixStoreToProject {
    NSURL *url = [self prefixStoreURL];
    NSURL *projDataURL = [self prefixStoreProjectDataURL];
    NSString *projDataPath = [projDataURL path];
    if ([_fops isFileExists:projDataPath]) {
        [_fops delete:projDataPath];
    }
    return [_fops copyFile:url toURL:projDataURL];
}

/*!
 Retrieves all prefixes from the Core Data store invoking the callback with the result once done.
 */
- (void)getPrefixes:(void(^)(NSArray<DLPrefix *> *))callback isSort:(BOOL)isSort {
    [DLLog debug:@"in getPrefixes method"];
    [self.prefixContainer performBackgroundTask:^(NSManagedObjectContext * bgMOC) {
        NSFetchRequest *req = [NSFetchRequest new];
        if (isSort) {
            NSSortDescriptor *desc = [NSSortDescriptor sortDescriptorWithKey:@"name" ascending:YES];
            [req setSortDescriptors:@[desc]];
        }
        [req setEntity:[NSEntityDescription entityForName:@"DLPrefix" inManagedObjectContext:bgMOC]];
        NSError *err;
        NSArray *prefixes = [bgMOC executeFetchRequest:req error:&err];
        if (err) {
            [DLLog errorWithFormat:@"Error fetching prefix from store: %@", err];
        }
        [DLLog debugWithFormat:@"getPrefix prefixes count: %ld", prefixes.count];
        callback(prefixes);
    }];
}

/*!
 Updates the state prefix trie with the prefixes loaded from the Core Data store.
 */
- (void)updateStateWithPrefix:(void(^ _Nullable)(BOOL))callback {
    [DLLog debug:@"in updateStateWithPrefix method"];
    [self getPrefixes:^(NSArray<DLPrefix *> *prefixes) {
        if (prefixes.count == 0 && callback) callback(false);
        [[[prefixes firstObject] managedObjectContext] performBlock:^{
            NSEnumerator *iter = [prefixes objectEnumerator];
            DLPrefix *prefix = nil;
            [DLLog debug:@"updating prefix tree"];
            while ((prefix = [iter nextObject]) != nil) {
                [DLState.shared.prefixTree insert:prefix.name];
            }
            [DLLog debug:@"State data updated with prefixes"];
            if (callback) callback(YES);
        }];
    } isSort:NO];
}

/*!
 Loads perfix from plist.
 */
- (NSArray *)loadPrefixFromPList {
    DLFileOps *fops = [DLFileOps new];
    NSString *prefixPlistPath = [[NSString alloc] initWithFormat:@"%@%@", fops.projectRoot, DLConst.prefixPlistPathFrag];
    NSData *plistData = [[NSData alloc] initWithContentsOfFile:prefixPlistPath];
    NSError *err;
    NSMutableDictionary *plist = [NSPropertyListSerialization propertyListWithData:plistData options:NSPropertyListImmutable format:nil error:&err];
    return [plist objectForKey:@"prefixes"];
}

@end
