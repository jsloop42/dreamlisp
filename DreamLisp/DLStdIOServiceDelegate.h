//
//  DLStdIOServiceDelegate.h
//  DreamLisp
//
//  Created by jsloop on 27/06/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/** Standard IO service delegate */
@protocol DLStdIOServiceDelegate <NSObject>
- (NSString *)readInput;
- (NSString *)readInputWithPrompt:(NSString *)prompt;
- (void)writeOutput:(NSString *)string;
- (void)writeOutput:(NSString *)string terminator:(NSString *)terminator;
@end

NS_ASSUME_NONNULL_END
