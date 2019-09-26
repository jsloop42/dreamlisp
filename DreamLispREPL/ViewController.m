//
//  ViewController.m
//  DreamLispREPL
//
//  Created by jsloop on 26/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "ViewController.h"

@interface ViewController ()

@end

@implementation ViewController {
    DreamLisp *_dl;
}

- (void)viewWillAppear:(BOOL)animated {
    [super viewWillAppear:animated];
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardWillShow:) name:UIKeyboardWillShowNotification object:nil];
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardWillHide:) name:UIKeyboardWillHideNotification object:nil];
}

- (void)viewWillDisappear:(BOOL)animated {
    [super viewWillDisappear:animated];
    [[NSNotificationCenter defaultCenter] removeObserver:self name:UIKeyboardWillShowNotification object:nil];
    [[NSNotificationCenter defaultCenter] removeObserver:self name:UIKeyboardWillHideNotification object:nil];
}

- (void)viewDidLoad {
    [super viewDidLoad];
    [self bootstrap];
}

- (void)bootstrap {
    [self initEvents];
    _dl = [[DreamLisp alloc] initWithoutREPL];
}

- (void)initEvents {
    [self.evalBtn addTarget:self action:@selector(evalBtnDidTap:) forControlEvents:UIControlEventTouchUpInside];
}

- (void)evalBtnDidTap:(UIButton *)sender {
    NSLog(@"eval btn did tap");
    [UIApplication.sharedApplication sendAction:@selector(resignFirstResponder) to:nil from:nil forEvent:nil];
    NSString *exp = [self.inputTextView text];
    NSString *ret = [_dl rep:exp];
    NSMutableString *outText = [[NSMutableString alloc] initWithString:[[self outputTextView] text]];
    [outText appendString:@"\n"];
    [outText appendString:ret];
    [self.outputTextView setText:outText];
}

#pragma mark - keyboard movements
- (void)keyboardWillShow:(NSNotification *)notification {
    NSLog(@"keyboard will show");
    CGSize keyboardSize = [[[notification userInfo] objectForKey:UIKeyboardFrameBeginUserInfoKey] CGRectValue].size;
    [UIView animateWithDuration:0.3 animations:^{
        CGRect f = self.view.frame;
        f.origin.y = -keyboardSize.height;
        self.view.frame = f;
    }];
}

-(void)keyboardWillHide:(NSNotification *)notification {
    NSLog(@"keyboard will hide");
    [UIView animateWithDuration:0.3 animations:^{
        CGRect f = self.view.frame;
        f.origin.y = 0.0f;
        self.view.frame = f;
    }];
}
@end

@implementation ViewController (UITextViewDelegate)

- (void)textViewDidBeginEditing:(UITextView *)textView {
    NSLog(@"text view did begin editing");
}

- (void)textViewDidEndEditing:(UITextView *)textView {
    NSLog(@"text view did end editing");
    [textView resignFirstResponder];
}

@end
