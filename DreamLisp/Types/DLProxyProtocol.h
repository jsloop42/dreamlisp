//
//  DLProxyProtocol.h
//  DreamLisp
//
//  Created by Jaseem V V on 14/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"

NS_ASSUME_NONNULL_BEGIN

@protocol DLProxyProtocol <DLDataProtocol>
/*! The key used in linking the proxy object as an associated object. */
@property (nonatomic, readwrite, retain, nullable) NSString *proxyAssocKey;
/*! The key used in linking the return object if present as an associated object. */
@property (nonatomic, readwrite, retain, nullable) NSString *returnAssocKey;
/*! The proxy object contained by the object references to. */
@property (nonatomic, readwrite, retain) id proxy;
/*!
 The return value associated with invoking the proxy object other than during an init. The init value of the proxy object can be obtained from the @c value
 property.
 */
@property (nonatomic, readwrite, assign) id returnValue;
+ (instancetype)new;
+ (NSString *)className;
- (instancetype)init;
- (instancetype)initWithProxy:(id)object;
- (NSString *)className;
- (void)forwardInvocation:(NSInvocation *)invocation;
- (NSMethodSignature *)methodSignatureForSelector:(SEL)sel;
@end

NS_ASSUME_NONNULL_END
