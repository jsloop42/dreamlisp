//
//  DLPrefix+CoreDataProperties.h
//  
//
//  Created by Jaseem V V on 24/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLPrefix+CoreDataClass.h"


NS_ASSUME_NONNULL_BEGIN

@interface DLPrefix (CoreDataProperties)

+ (NSFetchRequest<DLPrefix *> *)fetchRequest;

@property (nullable, nonatomic, copy) NSString *name;
@property (nullable, nonatomic, copy) NSDecimalNumber *weight;

@end

NS_ASSUME_NONNULL_END
