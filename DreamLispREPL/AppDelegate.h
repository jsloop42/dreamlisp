//
//  AppDelegate.h
//  DreamLispREPL
//
//  Created by Jaseem V V on 26/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <CoreData/CoreData.h>

@interface AppDelegate : UIResponder <UIApplicationDelegate>

@property (readonly, strong) NSPersistentCloudKitContainer *persistentContainer;

- (void)saveContext;


@end

