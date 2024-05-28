//
//  DLShellInput.h
//  DreamLispShell
//
//  Created by Jaseem V V on 19.05.2024.
//  Copyright © 2024 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/** Shell input result type */
@interface DLShellInput : NSObject
/** Flag indicating if the input expression is well formed */
@property (nonatomic, readwrite) BOOL shouldEvaluate;
/** The current line */
@property (nonatomic, strong) NSString *expr;
@end

NS_ASSUME_NONNULL_END
