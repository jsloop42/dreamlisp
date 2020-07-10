//
//  DLIOService.h
//  DreamLisp
//
//  Created by Jaseem V V on 27/06/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLFileIOServiceDelegate.h"
#import "DLStdIOServiceDelegate.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLIOService : NSObject <DLFileIOServiceDelegate, DLStdIOServiceDelegate>
@property (nonatomic, readwrite, weak) id<DLFileIOServiceDelegate> fileIODelegate;
@property (nonatomic, readwrite, weak) id<DLStdIOServiceDelegate> stdIODelegate;
- (instancetype)initWithFileIODelegate:(id<DLFileIOServiceDelegate>)aFileIODelegate stdIODelegate:(id<DLStdIOServiceDelegate>)aStdIODelegate;
- (NSBundle *)mainBundle;
- (NSString *)resourcePath;
@end

NS_ASSUME_NONNULL_END
