//
//  DLTerminal.h
//  DreamLispShell
//
//  Created by Jaseem V V on 08/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLStack.h"
#import "DLShellInput.h"
#import <Foundation/Foundation.h>
#import <readline/readline.h>
#import <DreamLisp/DreamLispLib.h>

NS_ASSUME_NONNULL_BEGIN

@interface DLTerminal : NSObject <DLStdIOServiceDelegate>
@property (nonatomic, readwrite, assign) const char *prompt;
@property (nonatomic, strong) DLStack *stack;
- (instancetype)init;
- (DLShellInput *)readline;
- (DLShellInput * _Nullable)readlineWithPrompt:(const char *)prompt;
- (void)loadHistoryFile:(NSString *)path;
- (void)disableHistory:(BOOL)flag;
- (const char *)promptWithModule:(NSString *)moduleName;
/**
 Checks if the given line is well formed. If parenthesis are matching, it will evaluate. Else the shell goes into multi-line mode till all open
 parenthesis are matched with closing one. The parenthesis encountered and the state is kept in the stack class.
 */
- (BOOL)shouldEvaluate:(NSString *)line;
@end

NS_ASSUME_NONNULL_END
