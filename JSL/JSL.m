//
//  JSL.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSL.h"

@implementation JSL {
    Reader *reader;
    Printer *printer;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        reader = [Reader new];
        printer = [Printer new];
    }
    return self;
}

- (JSData *)read:(NSString *)string {
    return [reader readString:string];
}

- (JSData *)eval:(JSData *)data {
    return data;
}

- (NSString *)print:(JSData *)data {
    return [printer printStringFor:data readably:YES];
}

- (NSString *)rep:(NSString *)string {
    return [self print:[self eval:[self read:string]]];
}

@end
