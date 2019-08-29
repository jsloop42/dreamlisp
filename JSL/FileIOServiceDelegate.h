//
//  FileIOServiceDelegate.h
//  JSL
//
//  Created by jsloop on 27/06/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/** File IO service delegate */
@protocol FileIOServiceDelegate <NSObject>
- (NSString *)readFile:(NSString *)path;
@end

NS_ASSUME_NONNULL_END
