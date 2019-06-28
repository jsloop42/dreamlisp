//
//  Terminal.h
//  JSLShell
//
//  Created by jsloop on 08/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <readline/readline.h>
#import <JSL/JSLLib.h>

NS_ASSUME_NONNULL_BEGIN

@interface Terminal : NSObject <StdIOServiceDelegate>
@property (nonatomic, readwrite) NSString *prompt;
- (instancetype)init;
- (NSString *)readline;
- (NSString * _Nullable)readlineWithPrompt:(const char *)prompt;
- (void)loadHistoryFile:(NSString *)path;
- (void)disableHistory:(BOOL)flag;
@end

NS_ASSUME_NONNULL_END
