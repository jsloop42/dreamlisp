//
//  IOService.h
//  DreamLisp
//
//  Created by jsloop on 27/06/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "FileIOServiceDelegate.h"
#import "StdIOServiceDelegate.h"

NS_ASSUME_NONNULL_BEGIN

@interface IOService : NSObject <FileIOServiceDelegate, StdIOServiceDelegate>
@property (nonatomic, readwrite, weak) id<FileIOServiceDelegate> fileIODelegate;
@property (nonatomic, readwrite, weak) id<StdIOServiceDelegate> stdIODelegate;
- (instancetype)initWithFileIODelegate:(id<FileIOServiceDelegate>)aFileIODelegate stdIODelegate:(id<StdIOServiceDelegate>)aStdIODelegate;
- (NSBundle *)mainBundle;
- (NSString *)resourcePath;
@end

NS_ASSUME_NONNULL_END
