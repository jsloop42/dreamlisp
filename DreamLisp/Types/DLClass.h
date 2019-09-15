//
//  DLClass.h
//  DreamLisp
//
//  Created by jsloop on 31/08/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLProxyProtocol.h"
#import "DLSymbol.h"
#import "DLSlot.h"
#import "DLKeyword.h"
#import "DLMethod.h"
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@class DLSymbol;
@class DLSlot;
@class DLKeyword;
@class DLMethod;

@interface DLClass : NSObject <DLProxyProtocol>
@property (nonatomic, readwrite, retain) DLSymbol *name;  /* The name of the class */
@property (nonatomic, readwrite, retain) NSMutableArray<DLSymbol *> *conformance;
@property (nonatomic, readwrite, retain) NSMutableArray<DLSlot *> *slots;
@property (nonatomic, readwrite, retain) NSMutableSet<DLMethod *> *methods;
+ (BOOL)isClass:(id)any;
+ (DLClass *)dataToClass:(id<DLDataProtocol>)data fnName:(NSString *)fnName;
+ (DLClass *)dataToClass:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (BOOL)containsSlotWithInitArg:(DLKeyword *)keyword;
- (DLSlot  * _Nullable)slotWithInitArg:(DLKeyword *)keyword;
@end

NS_ASSUME_NONNULL_END
