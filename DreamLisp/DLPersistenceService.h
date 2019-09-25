//
//  PersistenceService.h
//  DreamLisp
//
//  Created by jsloop on 23/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
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
+ (instancetype)shared;
- (BOOL)checkIfPrefixStoreExists;
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
