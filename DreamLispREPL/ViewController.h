//
//  ViewController.h
//  DreamLispREPL
//
//  Created by jsloop on 26/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <DreamLisp/DreamLispLib.h>

@interface ViewController : UIViewController <UITextViewDelegate>
@property (weak, nonatomic) IBOutlet UITextView *outputTextView;
@property (weak, nonatomic) IBOutlet UITextView *inputTextView;
@property (weak, nonatomic) IBOutlet UIButton *evalBtn;
@property (weak, nonatomic) IBOutlet NSLayoutConstraint *bottomConstraint;
@end

