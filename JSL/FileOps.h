//
//  FileOps.h
//  JSL
//
//  Created by jsloop on 08/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Utils.h"
#import "Logger.h"

NS_ASSUME_NONNULL_BEGIN

@interface FileOps : NSObject
- (instancetype)init;
- (void)createFileIfNotExist:(NSString *)path;
- (void)openFile:(NSString *)path;
- (void)closeFile;
- (BOOL)hashNext;
- (NSString *)readLine;
- (void)append:(NSString *)string completion:(void  (^ _Nullable)(void))callback;
- (BOOL)delete:(NSString *)path;
@end

NS_ASSUME_NONNULL_END
