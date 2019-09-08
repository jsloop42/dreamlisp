//
//  Network.m
//  DreamLisp
//
//  Created by jsloop on 08/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "Network.h"

static NSString *_description = @"The network module.";

@implementation Network {
    Env *_env;
    NetworkSessionTable *_networkSessionTable;
    NotificationTable *_notifTable;
    NSURLSession *_urlSession;
    NSOperationQueue *_queue;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (void)bootstrap {
    _env = [[Env alloc] init];
    [_env setModuleName:[Const networkModuleName]];
    [_env setModuleDescription:_description];
    [_env setIsUserDefined:NO];
    _queue = [[NSOperationQueue alloc] init];
    [_queue setName:@"network ops queue"];
    [_queue setQualityOfService:NSQualityOfServiceUserInitiated];
    NSURLSessionConfiguration *sessionConfig = [NSURLSessionConfiguration defaultSessionConfiguration];
    _urlSession = [NSURLSession sessionWithConfiguration:sessionConfig delegate:self delegateQueue:_queue];
    _networkSessionTable = NetworkSessionTable.shared;
    _notifTable = NotificationTable.shared;
}

#pragma mark - URLSession

- (void)addURLSessionFunctions {
    Network __weak *weakSelf = self;
    DLFunction *fn = nil;

#pragma mark - http-request
    /**
     Make an HTTP request with the given method type, delegate notification handler function, url and parameters and optional headers.

     (http-request :get :post-did-download "https://example.com/api/post" {:sort "latest" :offset 0 :max 100} {:content-type "application/json"})
     */
    id<DLDataProtocol>(^httpRequest)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            Network *this = weakSelf;
            [TypeUtils checkArity:xs arity:5];
            NSString *fnName = @"http-request/5";
            DLKeyword *methodType = [DLKeyword dataToKeyword:[xs first] position:0 fnName:fnName];
            DLKeyword *aNotifKey = [DLKeyword dataToKeyword:[xs second] position:1 fnName:fnName];
            DLString *urlString = [DLString dataToString:[xs nth:2] position:2 fnName:fnName];
            DLHashMap *data = [DLHashMap dataToHashMap:[xs nth:3] position:3 fnName:fnName];  /* Query param or request body */
            DLHashMap *headers = [xs nth:4];
            if ([methodType isEqualToString:@"get"]) {
                NSURLComponents *comp = [[NSURLComponents alloc] initWithString:[urlString value]];
                NSMutableArray <NSURLQueryItem *> *queryItems = [NSMutableArray new];
                NSURLQueryItem *queryItem = nil;
                NSArray *allKeys = [data allKeys];
                NSUInteger len = [allKeys count];
                NSUInteger i = 0;
                DLKeyword *key = nil;
                DLString *val = nil;
                for (i = 0; i < len; i++) {
                    key = [allKeys objectAtIndex:i];
                    val = [data objectForKey:key];
                    queryItem = [[NSURLQueryItem alloc] initWithName:[key string] value:[val value]];
                    [queryItems addObject:queryItem];
                }
                [comp setQueryItems:queryItems];
                NSMutableURLRequest *req = [NSMutableURLRequest requestWithURL:[comp URL]];
                [req setHTTPMethod:@"GET"];
                [req setAllHTTPHeaderFields:[[Utils convertKeywordKeysToString:headers] value]];
                NSURLSessionDataTask *task = [this->_urlSession dataTaskWithRequest:req];
                NotificationData *notifData = [this->_notifTable notification:aNotifKey];
                notifData.identifier = [[NSNumber alloc] initWithInteger:task.taskIdentifier];
                [this->_networkSessionTable setNotification:notifData];
                [task resume];
            } else if ([methodType isEqualToString:@"post"] || [methodType isEqualToString:@"put"] || [methodType isEqualToString:@"patch"] || [methodType isEqualToString:@"delete"]) {
                NSURL *url = [NSURL URLWithString:[urlString value]];
                NSMutableURLRequest *req = [NSMutableURLRequest requestWithURL:url];
                [req setHTTPMethod:[Utils httpMethodTypeToString:methodType]];
                [req setAllHTTPHeaderFields:[[Utils convertKeywordKeysToString:headers] value]];
                if (data && [data count] > 0) {
                    NSMutableDictionary *dict = [Utils hashMapToFoundationType:data]; /* Convert to Foundation type */
                    NSData *jsonData = [Utils encodeDictionaryToJSONData:dict];  /* JSON encode */
                    [req setHTTPBody:jsonData];
                }
                NSURLSessionDataTask *task = [this->_urlSession dataTaskWithRequest:req];
                NotificationData *notifData = [this->_notifTable notification:aNotifKey];
                notifData.identifier = [[NSNumber alloc] initWithInteger:task.taskIdentifier];
                [this->_networkSessionTable setNotification:notifData];
                [task resume];
            }
            return [[DLBool alloc] initWithBool:YES];
        }
    };
    fn = [[DLFunction alloc] initWithFn:httpRequest argCount:1 name:@"http-request/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"http-request" moduleName:[Const coreModuleName]]];
}

#pragma mark - Delegates

- (void)URLSession:(NSURLSession *)session dataTask:(NSURLSessionDataTask *)dataTask didReceiveResponse:(NSURLResponse *)response
 completionHandler:(void (^)(NSURLSessionResponseDisposition))completionHandler {
    completionHandler(NSURLSessionResponseAllow);
}

- (void)URLSession:(NSURLSession *)session dataTask:(NSURLSessionDataTask *)dataTask didReceiveData:(NSData *)data {
    NSLog(@"Data did receive: %@", [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding]);
    NSMutableDictionary *hm = [NSMutableDictionary new];
    DLData *dlData = nil;
    if (data) dlData = [[DLData alloc] initWithData:data];
    [hm setObject:dlData ? dlData : [DLNil new] forKey:[[DLKeyword alloc] initWithString:@"data"]];
    DLHashMap *respHM = [[DLHashMap alloc] initWithDictionary:[[(NSHTTPURLResponse *)dataTask.response allHeaderFields] mutableCopy]];
    NSLog(@"response data string: %@", [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding]);
    [hm setObject:respHM forKey:[[DLKeyword alloc] initWithString:@"response"]];
    [hm setObject:[Utils errorToHashMap:dataTask.error] forKey:[DLKeyword keywordWithString:@"error"]];
    NotificationData *notifData = [_networkSessionTable notification:[[NSNumber alloc] initWithInteger:dataTask.taskIdentifier]];
    [[NSNotificationCenter defaultCenter] postNotificationName:[notifData.notificationKey value] object:self
                                                      userInfo:@{@"notifKey": notifData.notificationKey, @"params": [@[hm] mutableCopy]}];

}

- (void)URLSession:(NSURLSession *)session task:(NSURLSessionTask *)task didCompleteWithError:(NSError *)error {
    if (error) {
        NSLog(@"Data error: %@", error.description);
    } else {
        NSLog(@"Data download success: %@", error.description);
    }
}

@end
