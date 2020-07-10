//
//  PersistenceService.h
//  DreamLisp
//
//  Created by Jaseem V V on 23/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>
#import "DLConst.h"
#import "DLLogger.h"
#import "DLFileOps.h"
#import "DLState.h"
#import "DLPrefix+CoreDataClass.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLPersistenceService : NSObject
@property (nonatomic, readwrite, retain) NSPersistentStoreCoordinator *prefixStoreCoordinator;
@property (nonatomic, readwrite, retain) NSPersistentStore *prefixStore;
@property (nonatomic, readwrite, retain) NSPersistentContainer *prefixContainer;
@property (nonatomic, readonly, retain) NSManagedObjectContext *prefixMOC;
@property (nonatomic, readonly, assign) BOOL isPrefixStoreInitialized;
+ (instancetype)shared;
- (BOOL)checkIfPrefixStoreExists;
- (BOOL)deletePrefixStore;
- (void)initPersistence:(void(^)(BOOL))callback;
- (NSURL *)prefixStoreBundleURL;
- (void)initPrefixStore:(void (^)(void))callback;
- (NSURL *)prefixStoreURL;
- (void)insertPrefixToStoreInBatch:(void(^)(BOOL))callback;
- (NSURL *)prefixStoreProjectDataURL;
- (BOOL)copyPrefixStoreToProject;
- (void)getPrefixes:(void(^)(NSArray<DLPrefix *> *))callback isSort:(BOOL)isSort;
- (void)updateStateWithPrefix:(void(^ _Nullable)(BOOL))callback;
- (NSArray *)loadPrefixFromPList;
@end

NS_ASSUME_NONNULL_END
