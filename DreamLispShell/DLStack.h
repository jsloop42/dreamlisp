//
//  DLStack.h
//  DreamLispShell
//
//  Created by Jaseem V V on 18.05.2024.
//  Copyright © 2024 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/** Class to keep track of symbols for multi-line support in terminal. */
@interface DLStack : NSObject
/** If currently within a string. */
@property (nonatomic, readwrite) BOOL isInStringMode;
/** Add an element to the stack. */
- (void)push:(NSString *)elem;
/** Remove the last added element from the stack. */
- (NSString * _Nullable)pop;
/** Get the latest element on the stack. */
- (NSString * _Nullable)top;
/** Pop characters still the given element is encountered, including the element. */
- (void)popTill:(NSString *)elem;
/** Get the stack length. */
- (NSUInteger)count;
/** Is the stack empty. */
- (BOOL)isEmpty;
/** Clear all state. */
- (void)reset;
@end

NS_ASSUME_NONNULL_END
