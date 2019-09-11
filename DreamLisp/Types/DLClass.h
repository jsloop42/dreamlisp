//
//  DLClass.h
//  DreamLisp
//
//  Created by jsloop on 31/08/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLSymbol.h"
#import "DLSlot.h"
#import "DLKeyword.h"
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@class DLSymbol;
@class DLSlot;
@class DLKeyword;

@interface DLClass : NSObject <DLDataProtocol>
@property (nonatomic, readwrite, retain) Class value;
@property (nonatomic, readwrite, retain) DLSymbol *name;  /* The name of the class */
@property (nonatomic, readwrite, retain) NSMutableArray<DLSymbol *> *conformance;
@property (nonatomic, readwrite, retain) NSMutableArray<DLSlot *> *slots;
+ (BOOL)isClass:(id)any;
- (instancetype)init;
- (instancetype)initWithClass:(Class)cls;
- (BOOL)containsSlotWithInitArg:(DLKeyword *)keyword;
- (DLSlot  * _Nullable)slotWithInitArg:(DLKeyword *)keyword;
@end

NS_ASSUME_NONNULL_END
