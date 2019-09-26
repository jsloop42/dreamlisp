//
//  AppDelegate.h
//  DreamLispREPL
//
//  Created by jsloop on 26/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <CoreData/CoreData.h>

@interface AppDelegate : UIResponder <UIApplicationDelegate>

@property (readonly, strong) NSPersistentCloudKitContainer *persistentContainer;

- (void)saveContext;


@end

