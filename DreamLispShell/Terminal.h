//
//  Terminal.h
//  DreamLispShell
//
//  Created by jsloop on 08/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <readline/readline.h>
#import <DreamLisp/DreamLispLib.h>

NS_ASSUME_NONNULL_BEGIN

@interface Terminal : NSObject <StdIOServiceDelegate>
@property (nonatomic, readwrite, assign) const char *prompt;
- (instancetype)init;
- (NSString *)readline;
- (NSString * _Nullable)readlineWithPrompt:(const char *)prompt;
- (void)loadHistoryFile:(NSString *)path;
- (void)disableHistory:(BOOL)flag;
- (const char *)promptWithModule:(NSString *)moduleName;
@end

NS_ASSUME_NONNULL_END
