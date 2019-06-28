//
//  StdIOServiceDelegate.h
//  JSL
//
//  Created by jsloop on 27/06/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/** Standard IO service delegate */
@protocol StdIOServiceDelegate <NSObject>
- (NSString *)readInput;
- (NSString *)readInputWithPrompt:(NSString *)prompt;
- (void)writeOutput:(NSString *)string;
@end

NS_ASSUME_NONNULL_END
