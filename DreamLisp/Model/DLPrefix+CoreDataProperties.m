//
//  DLPrefix+CoreDataProperties.m
//  
//
//  Created by jsloop on 24/09/19.
//
//

#import "DLPrefix+CoreDataProperties.h"

@implementation DLPrefix (CoreDataProperties)

+ (NSFetchRequest<DLPrefix *> *)fetchRequest {
	return [NSFetchRequest fetchRequestWithEntityName:@"DLPrefix"];
}

@dynamic name;
@dynamic weight;

@end
