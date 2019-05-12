//
//  JSL.h
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Types.h"
#import "Reader.h"
#import "Printer.h"
#import "Core.h"
#import <objc/objc-api.h>

NS_ASSUME_NONNULL_BEGIN

@interface JSL : NSObject
@property (nonatomic, readwrite) Env *globalEnv;
@property (nonatomic, readwrite) Env *env;
@property (nonatomic, readwrite) NSMapTable<NSString *, ModuleTable *> *modules;
- (instancetype)init;
- (NSString * _Nullable)rep:(NSString *)string;
- (NSString * _Nullable)printException:(NSException *)exception log:(BOOL)log readably:(BOOL)readably;
@end

NS_ASSUME_NONNULL_END
