//
//  DLPersistenceService.m
//  DreamLisp
//
//  Created by jsloop on 23/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLPersistenceService.h"

@interface DLPersistenceService ()
@property (nonatomic, readwrite, retain) NSPersistentStoreCoordinator *prefixStoreCoordinator;
@end

@implementation DLPersistenceService {
    NSPersistentContainer *_prefixContainer;
    NSManagedObjectContext *_prefixMOC;
    DLFileOps *_fops;
}

@synthesize prefixMOC = _prefixMOC;
@synthesize prefixStoreCoordinator;

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (void)bootstrap {
    _fops = [DLFileOps new];
}

- (BOOL)checkIfPrefixStoreExists {
    return NO;
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
//    _prefixContainer = [[NSPersistentContainer alloc] initWithName:DLConst.prefixStoreName];
//    [_prefixContainer loadPersistentStoresWithCompletionHandler:^(NSPersistentStoreDescription *description, NSError *err) {
//        if (err) {
//            [DLLog errorWithFormat:@"Failed to load prefix store: %@", err];
//            return;
//        }
//        callback();
//    }];

    NSURL *prefixMOMD = [[NSBundle bundleForClass:[self class]] URLForResource:DLConst.prefixStoreName withExtension:@"momd"];
    if (!prefixMOMD) {
        [DLLog error:@"Prefix MOMD file not found"];
        return;
    }
    NSManagedObjectModel *mom = [[NSManagedObjectModel alloc] initWithContentsOfURL:prefixMOMD];
    if (!mom) {
        [DLLog error:@"Managed Object Model initialization failed"];
        return;
    }
    NSPersistentStoreCoordinator *coordinator = [[NSPersistentStoreCoordinator alloc] initWithManagedObjectModel:mom];
    NSManagedObjectContext *moc = [[NSManagedObjectContext alloc] initWithConcurrencyType:NSPrivateQueueConcurrencyType];
    [moc setPersistentStoreCoordinator:coordinator];
    _prefixMOC = moc;
//    NSURL *storeURL = [NSURL URLWithString:@"/Users/jsloop/temp/dlprefix.sqlite"];
//    NSError *err;
    //NSPersistentStore *store = [coordinator addPersistentStoreWithType:NSSQLiteStoreType configuration:nil URL:nil options:nil error:&err];
//    if (!store) {
//        NSLog(@"error %@: ", err);
//    }
    DLPersistenceService __weak *weakSelf = self;
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_BACKGROUND, 0), ^{
        DLPersistenceService *this = weakSelf;
        NSPersistentStoreCoordinator *psc = [self.prefixMOC persistentStoreCoordinator];
        //NSPersistentStore *store = [[NSPersistentStore alloc] initWithPersistentStoreCoordinator:psc configurationName:DLConst.prefixStoreName URL:[NSURL URLWithString:@"/Users/jsloop/temp/dlispstore.sqlite"] options:nil];

        NSError *err;
        NSPersistentStore *store = [psc addPersistentStoreWithType:NSSQLiteStoreType configuration:nil URL:[self prefixStoreURL] options:nil error:&err];
        if (!store) {
            [DLLog errorWithFormat:@"Failed to initialize prefix store: %@", err.description];
            return;
        }
        [[this->_prefixContainer viewContext] setAutomaticallyMergesChangesFromParent:YES];
        if (!callback) return;
        dispatch_async(dispatch_get_main_queue(), ^{
            callback();
        });
    });
}

/*!
 Returns the prefix store URL. If the path to the store does not exist, it's created.
 */
- (NSURL *)prefixStoreURL {
    NSURL *docURL = [_fops applicationSupportDirectory];
    NSBundle *bundle = [NSBundle bundleForClass:[self class]];
    NSDictionary *info = [bundle infoDictionary];
    NSString *bundleID = [info objectForKey:@"CFBundleIdentifier"];
    NSURL *storeDirURL = [docURL URLByAppendingPathComponent: bundleID];
    NSString *storeDirPath = [storeDirURL path];
    if ([_fops isDirectoryExists:storeDirPath]) {
        [_fops createDirectoryWithIntermediate:storeDirPath];
    }
    return [storeDirURL URLByAppendingPathComponent:[[NSString alloc] initWithFormat:@"/%@.sqlite", DLConst.prefixStoreName]];
}

- (void)insertPrefixToStoreInBatch:(void(^)(BOOL))callback {
    [_prefixContainer performBackgroundTask:^(NSManagedObjectContext *bgMOC) {
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

- (BOOL)copyPrefixStoreToProject {
    NSURL *url = [self prefixStoreURL];
    NSMutableString *destStr = [NSMutableString new];
    [destStr appendString:[_fops projectRoot]];
    [destStr appendString:@"/data/"];
    [destStr appendString:DLConst.prefixStoreName];
    return [_fops copyFile:url toURL:[NSURL URLWithString:destStr]];
}

- (void)updateStateWithPrefix {
    /*
     1. Pre-req: Load the prefix store
     2. Get all prefixes and update the trie
     */
    [_prefixContainer performBackgroundTask:^(NSManagedObjectContext * bgMOC) {
        NSFetchRequest *req = [NSFetchRequest new];
        [req setEntity:[NSEntityDescription entityForName:@"DLPrefix" inManagedObjectContext:bgMOC]];
        NSError *err;
        NSArray *prefixes = [bgMOC executeFetchRequest:req error:&err];
        if (err) {
            [DLLog errorWithFormat:@"Error fetching prefix from store: %@", err];
        }
        NSEnumerator *iter = [prefixes objectEnumerator];
        DLPrefix *prefix = nil;
        while ((prefix = [iter nextObject]) != nil) {
            [DLState.shared.prefixTree insert:prefix.name];
        }
    }];
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
