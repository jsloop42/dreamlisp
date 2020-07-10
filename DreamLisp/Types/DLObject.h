//
//  DLObject.h
//  DreamLisp
//
//  Created by Jaseem V V on 01/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLProxyProtocol.h"
#import "DLClass.h"
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@class DLClass;

@interface DLObject : NSProxy <DLProxyProtocol>
@property (nonatomic, readwrite, retain) DLClass *cls;
/*! The object that the class holds */
+ (BOOL)isObject:(id)any;
+ (DLObject *)dataToObject:(id<DLDataProtocol>)data fnName:(NSString *)fnName;
+ (DLObject *)dataToObject:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
@end

NS_ASSUME_NONNULL_END
