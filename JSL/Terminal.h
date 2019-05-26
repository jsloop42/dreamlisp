//
//  Terminal.h
//  JSL
//
//  Created by jsloop on 08/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <readline/readline.h>
#import "FileOps.h"
#import "Utils.h"
#import "Logger.h"
#import "ModuleTable.h"

NS_ASSUME_NONNULL_BEGIN


@interface Terminal : NSObject
@property (nonatomic, readwrite) NSString *prompt;
- (instancetype)init;
- (NSString *)readline;
- (NSString * _Nullable)readlineWithPrompt:(const char *)prompt;
- (void)loadHistoryFile:(NSString *)path;
- (void)disableHistory:(BOOL)flag;
@end

NS_ASSUME_NONNULL_END
