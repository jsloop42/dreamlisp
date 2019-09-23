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
@property (nonatomic, readonly, retain) NSManagedObjectContext *prefixMOC;
- (void)initPrefixStore:(void (^)(void))callback;
- (void)insertPrefixToStoreInBatch:(void(^)(BOOL))callback;
- (void)updateStateWithPrefix;
- (NSArray *)loadPrefixFromPList;
@end

NS_ASSUME_NONNULL_END
