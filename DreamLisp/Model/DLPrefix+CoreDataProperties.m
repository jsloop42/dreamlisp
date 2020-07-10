//
//  DLPrefix+CoreDataProperties.m
//  
//
//  Created by Jaseem V V on 24/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLPrefix+CoreDataProperties.h"

@implementation DLPrefix (CoreDataProperties)

+ (NSFetchRequest<DLPrefix *> *)fetchRequest {
	return [NSFetchRequest fetchRequestWithEntityName:@"DLPrefix"];
}

@dynamic name;
@dynamic weight;

@end
