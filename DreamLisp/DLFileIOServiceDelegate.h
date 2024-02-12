//
//  DLFileIOServiceDelegate.h
//  DreamLisp
//
//  Created by Jaseem V V on 27/06/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/** File IO service delegate */
@protocol DLFileIOServiceDelegate <NSObject>
- (NSString *)readFile:(NSString *)path;
@end

NS_ASSUME_NONNULL_END
