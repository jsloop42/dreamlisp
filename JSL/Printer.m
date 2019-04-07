//
//  Printer.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Printer.h"

@implementation Printer

- (NSString *)printListFor:(JSList *)list readably:(BOOL)readably withFormatter:(NSString *)formatter {
    NSMutableArray *xs = [(JSList *)list map:^NSString *(JSData *obj) {
        return [self printStringFor:obj readably:readably];
    }];
    return [[NSString alloc] initWithFormat:formatter, [xs componentsJoinedByString:@" "]];
}

- (NSString *)printStringFor:(JSData *)data readably:(BOOL)readably {
    NSString *dataType = [data dataType];
    if ([dataType isEqual:@"JSSymbol"]) {
        return [(JSSymbol *)data name];
    } else if ([dataType isEqual:@"JSNumber"]) {
        return [(JSNumber *)data string];
    } else if ([dataType isEqual:@"JSString"]) {
        NSString *string = [(JSString *)data value];
        if (readably) {
            NSMutableString *ms = [[NSMutableString alloc] initWithString:@"\""];
            NSString *readable = [[[string stringByReplacingOccurrencesOfString:@"\\" withString:@"\\\\"]
                                   stringByReplacingOccurrencesOfString:@"\"" withString:@"\\\""]
                                   stringByReplacingOccurrencesOfString:@"\n" withString:@"\\n"];
            [ms appendString:readable];
            [ms appendString:@"\""];
            return ms;
        }
        return string;
    } else if ([dataType isEqual:@"JSList"]) {
        return [self printListFor:(JSList *)data readably:readably withFormatter:@"(%@)"];
    } else if ([dataType isEqual:@"JSVector"]) {
        return [self printListFor:(JSVector *)data readably:readably withFormatter:@"[%@]"];
    } else if ([dataType isEqual:@"JSHashMap"]) {
        NSMutableArray *xs = [(JSHashMap *)data map:^NSString *(JSData *key, JSData * val) {
            return [[NSString alloc] initWithFormat:@"%@ %@", [self printStringFor:key readably:readably], [self printStringFor:val readably:readably]];
        }];
        return [[NSString alloc] initWithFormat:@"{%@}", [xs componentsJoinedByString:@" "]];
    } else if ([dataType isEqual:@"JSKeyword"]) {
        return [(JSKeyword *)data value];
    } else if ([dataType isEqual:@"JSAtom"]) {
        return [self printStringFor:[[JSString alloc] initWithFormat:@"(atom %@)", [(JSAtom *)data value]] readably:readably];
    } else if ([dataType isEqual:@"JSNil"]) {
        return @"nil";
    } else if ([dataType isEqual:@"JSTrue"]) {
        return @"true";
    } else if ([dataType isEqual:@"JSFalse"]) {
        return @"false";
    } else if ([dataType isEqual:@"JSFunction"]) {
        return @"#<function>";
    }
    return ERROR_TYPE;
}
@end
