//
//  DLNetwork.m
//  DreamLisp
//
//  Created by Jaseem V V on 08/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLNetwork.h"

static NSString *_description = @"The network module.";

@implementation DLNetwork {
    DLEnv *_env;
    DLNetworkSessionTable *_networkSessionTable;
    DLNotificationTable *_notifTable;
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
    _env = [[DLEnv alloc] init];
    [_env setModuleName:[DLConst networkModuleName]];
    [_env setModuleDescription:_description];
    [_env setIsUserDefined:NO];
    [_env setIsExportAll:YES];
    _queue = [[NSOperationQueue alloc] init];
    [_queue setName:@"network ops queue"];
    [_queue setQualityOfService:NSQualityOfServiceUserInitiated];
    NSURLSessionConfiguration *sessionConfig = [NSURLSessionConfiguration defaultSessionConfiguration];
    _urlSession = [NSURLSession sessionWithConfiguration:sessionConfig delegate:self delegateQueue:_queue];
    _networkSessionTable = DLNetworkSessionTable.shared;
    _notifTable = DLNotificationTable.shared;
    [self addURLSessionFunctions];
}

#pragma mark - URLSession

- (void)addURLSessionFunctions {
    DLNetwork __weak *weakSelf = self;
    DLFunction *fn = nil;

    #pragma mark - http-request
    /**
     Make an HTTP request with the given method type, delegate notification handler function, url and parameters and optional headers.

     (http-request :get :post-did-download "https://example.com/api/post" {:sort "latest" :offset 0 :max 100} {:content-type "application/json"})
     */
    id<DLDataProtocol>(^httpRequest)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            DLNetwork *this = weakSelf;
            [DLTypeUtils checkArity:xs arity:5];
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
                DLHashMap *hm = [DLUtils convertKeywordKeysToString:headers];
                NSMutableDictionary *headersDict = [DLUtils mapTableToDictionary:[hm value]];
                [req setAllHTTPHeaderFields:headersDict];
                NSURLSessionDataTask *task = [this->_urlSession dataTaskWithRequest:req];
                DLNotificationData *notifData = [this->_notifTable notification:aNotifKey];
                notifData.identifier = [[NSNumber alloc] initWithInteger:task.taskIdentifier];
                [this->_networkSessionTable setNotification:notifData];
                [task resume];
            } else if ([methodType isEqualToString:@"post"] || [methodType isEqualToString:@"put"] || [methodType isEqualToString:@"patch"] || [methodType isEqualToString:@"delete"]) {
                NSURL *url = [NSURL URLWithString:[urlString value]];
                NSMutableURLRequest *req = [NSMutableURLRequest requestWithURL:url];
                [req setHTTPMethod:[DLUtils httpMethodTypeToString:methodType]];
                DLHashMap *hm = [DLUtils convertKeywordKeysToString:headers];
                NSMutableDictionary *headersDict = [DLUtils mapTableToDictionary:[hm value]];
                [req setAllHTTPHeaderFields:headersDict];
                if (data && [data count] > 0) {
                    NSMutableDictionary *dict = [DLUtils hashMapToFoundationType:data]; /* Convert to Foundation type */
                    NSData *jsonData = [DLUtils encodeDictionaryToJSONData:dict];  /* JSON encode */
                    [req setHTTPBody:jsonData];
                }
                NSURLSessionDataTask *task = [this->_urlSession dataTaskWithRequest:req];
                DLNotificationData *notifData = [this->_notifTable notification:aNotifKey];
                notifData.identifier = [[NSNumber alloc] initWithInteger:task.taskIdentifier];
                [this->_networkSessionTable setNotification:notifData];
                [task resume];
            }
            return [[DLBool alloc] initWithBool:YES];
        }
    };
    fn = [[DLFunction alloc] initWithFn:httpRequest argCount:5 name:@"http-request/5"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"http-request" moduleName:[_env moduleName]]];
}

#pragma mark - Delegates

- (void)URLSession:(NSURLSession *)session dataTask:(NSURLSessionDataTask *)dataTask didReceiveResponse:(NSURLResponse *)response
 completionHandler:(void (^)(NSURLSessionResponseDisposition))completionHandler {
    completionHandler(NSURLSessionResponseAllow);
}

- (void)URLSession:(NSURLSession *)session dataTask:(NSURLSessionDataTask *)dataTask didReceiveData:(NSData *)data {
    DLHashMap *hm = [DLHashMap new];
    DLData *dlData = nil;
    if (data) dlData = [[DLData alloc] initWithData:data];
    [hm setObject:dlData ? dlData : [DLNil new] forKey:[[DLKeyword alloc] initWithString:@"data"]];
    DLHashMap *respHeaders = [DLUtils convertFromFoundationTypeToDLType:[[(NSHTTPURLResponse *)dataTask.response allHeaderFields] mutableCopy]];
    [hm setObject:respHeaders forKey:[[DLKeyword alloc] initWithString:@"response-headers"]];
    NSError *err = dataTask.error;
    [hm setObject:err ? [DLUtils errorToHashMap:err] : [DLNil new] forKey:[DLKeyword keywordWithString:@"error"]];
    DLNotificationData *notifData = [_networkSessionTable notification:[[NSNumber alloc] initWithInteger:dataTask.taskIdentifier]];
    [[NSNotificationCenter defaultCenter] postNotificationName:[notifData.notificationKey value] object:self
                                                      userInfo:@{DLConst.keyForNotificationKey: notifData.notificationKey,
                                                                 DLConst.keyForNotificationValue: [@[hm] mutableCopy]}];

}

- (void)URLSession:(NSURLSession *)session task:(NSURLSessionTask *)task didCompleteWithError:(NSError *)error {
    if (error) {
        [[[DLError alloc] initWithDescription:error.description] throw];
    }
}

@end
