//===-- IPConstantPropagation.cpp - Propagate constants through calls -----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass implements an _extremely_ simple interprocedural constant
// propagation pass.  It could certainly be improved in many different ways,
// like using a worklist.  This pass makes arguments dead, but does not remove
// them.  The existing dead argument elimination pass should be run after this
// to clean up the mess.
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/IPO.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "ipco"

STATISTIC(NumArgumentsProped, "Number of args turned into constants");
STATISTIC(NumReturnValProped, "Number of return values turned into constants");

namespace {
  /// IPCP - The interprocedural constant propagation pass
  ///
  struct IPCP : public ModulePass {
    static char ID; // Pass identification, replacement for typeid
    IPCP() : ModulePass(ID) {
      initializeIPCPPass(*PassRegistry::getPassRegistry());
    }

    bool runOnModule(Module &M) override;
  private:
    bool PropagateConstantsIntoArguments(Function &F);
    bool PropagateConstantReturn(Function &F);
  };
}

char IPCP::ID = 0;

//INITIALIZE_PASS(IPCP, "ipconstprop",
//               "Interprocedural constant propagation", false, false)

static RegisterPass<IPCP> X("ipco","EECS 583 Interprocedural constant prop", false, false);

ModulePass *llvm::createIPConstantPropagationPass() { return new IPCP(); }

bool IPCP::runOnModule(Module &M) {
  bool Changed = false;
  bool LocalChange = true;
  errs()<<"Interprocedure Consant Propagation  Pass running... " << '\n';

  // FIXME: instead of using smart algorithms, we just iterate until we stop
  // making changes.
  while (LocalChange) {
    LocalChange = false;
    for (Module::iterator I = M.begin(), E = M.end(); I != E; ++I)
      // isDeclaration(): If the function is not in the current function is not in the current 
      // translation unit. 
      if (!I->isDeclaration()) {

  errs() << "isDeclaration" << '\n';
        // Delete any klingons.
        // removeDeadConstantUsers(): I think it checks for livness with the constants
        // constants and its uses
        I->removeDeadConstantUsers();

        // hasLocalLinkage(): it determines what kind of linkage, Linkage in llvm are enums
        // Linkage(From wikipedia): describes how names can or can not refer to the same 
        // entity throughout the whole program or single translation unit.Like how static variables 
        // can be reference in different files. Linkage is basically a mechanism to allow var/func/etc
        // to be used and referenced in other source files. I think. 
        if (I->hasLocalLinkage()){
          
          errs() << "Has local linkgae" << I->hasLocalLinkage() <<'\n';
          LocalChange |= PropagateConstantsIntoArguments(*I);
        }
        Changed |= PropagateConstantReturn(*I);
      }
    Changed |= LocalChange;
  }
  return Changed;
}

/// PropagateConstantsIntoArguments - Look at all uses of the specified
/// function.  If all uses are direct call sites, and all pass a particular
/// constant in for an argument, propagate that constant in as the argument.
///
bool IPCP::PropagateConstantsIntoArguments(Function &F) {
  errs() << "I am here" << '\n';
  // From Docs: Every value has a "use list"(UseList) that keeps track of which other Values are
  // using this Value. 
  // llvm::Value::use_empty(): returns true if there are no users of this function(Value)
  if (F.arg_empty() || F.use_empty()) return false; // No arguments? Early exit.

  // For each argument, keep track of its constant value and whether it is a
  // constant or not.  The bool is driven to true when found to be non-constant.

  // SmallVector Class: This is a 'vector' (really, a variable-sized array), optimized for the case when the array is small.
  // It contains some number of elements in-place, which allows it to avoid heap allocation when the actual 
  // number of elements is below that threshold. 
  // This allows normal "small" cases to be fast without losing generality for large inputs.
  SmallVector<std::pair<Constant*, bool>, 16> ArgumentConstants;
  ArgumentConstants.resize(F.arg_size());

  unsigned NumNonconstant = 0;
  
  // Def use chain for finding all of the functions that use func F
  // The list of all Users of a particular Value is called a def-use chain. 
  // For example, let’s say we have a Function* named F to a particular function foo. 
  // Finding all of the instructions that use foo is as simple as iterating over the def-use chain of F

  // A Use represents the edge between a Value definition and its users. 
  for (Use &U : F.uses()) {
    
    // Returns the User that contains this Use.
    // For an instruction operand, for example, this will return the instruction. 
    User *UR = U.getUser();
/*    errs() << "Func: " << F.getName() << '\n';
    if (Instruction *Inst = dyn_cast<Instruction>(UR)) {
      errs() << "F is used in instruction:\n";
      errs() << *Inst << "\n";
    }
 
    errs() << "User: " << *UR << '\n';
    errs() << "Use: " << *U << '\n';
*/
    // isa<>:
    // The isa<> operator works exactly like the Java “instanceof” operator. 
    // It returns true or false depending on whether a reference or pointer points to an instance of the specified class. 
    // This can be very useful for constraint checking of various sorts.

    // Ignore blockaddress uses.
    if (isa<BlockAddress>(UR)) continue;
    
    // Used by a non-instruction, or not the callee of a function, do not
    // transform.
    if (!isa<CallInst>(UR) && !isa<InvokeInst>(UR)){
      errs() << "The User is not a CallInst or InvokeInst" << '\n';
      return false;
    }

    // llvm::CallSite: which is a handy wrapper class for code/instructions that
    // wants to treat Call and Invoke instructions in a generic way.    
    CallSite CS(cast<Instruction>(UR));

    // Checks if the the Use U (the edge between the function and its user) is a Callee.
    // Basically, if the User is a callee function if not then return. 
    if (!CS.isCallee(&U)){
      errs() << "Not a call site" << '\n';
      return false;
    }

    // Check out all of the potentially constant arguments.  Note that we don't
    // inspect varargs here.

    // For the call site, grab an iterater for its arguments
    CallSite::arg_iterator AI = CS.arg_begin();
  
    // For function, grab an iterator for its arguments
    Function::arg_iterator Arg = F.arg_begin();

    // Loop through the struct that contains the constants (potential) 
    for (unsigned i = 0, e = ArgumentConstants.size(); i != e;
         ++i, ++AI, ++Arg) {
      
      // If this argument is known non-constant, ignore it.
      if (ArgumentConstants[i].second)
        continue;
      
      Constant *C = dyn_cast<Constant>(*AI);
      if (C && ArgumentConstants[i].first == nullptr) {
        ArgumentConstants[i].first = C;   // First constant seen.
      } else if (C && ArgumentConstants[i].first == C) {
        // Still the constant value we think it is.
      } else if (*AI == &*Arg) {
        // Ignore recursive calls passing argument down.
      } else {
        // Argument became non-constant.  If all arguments are non-constant now,
        // give up on this function.
        if (++NumNonconstant == ArgumentConstants.size())
          return false;
        // false == constant
        // true == not constant
        ArgumentConstants[i].second = true;
      }
    }
  }

  // If we got to this point, there is a constant argument!
  assert(NumNonconstant != ArgumentConstants.size());
    errs() << "Yay something happen" << '\n';
  bool MadeChange = false;
  
  // Loop through each of the function's args
  Function::arg_iterator AI = F.arg_begin();
  for (unsigned i = 0, e = ArgumentConstants.size(); i != e; ++i, ++AI) {
    //  Do we have a constant argument?
    
    //  If the argument if not a constant, or
    //  if the argument has no uses(aka nobody uses it)  or 
    //  If the argument has the inalloca attribute or
    //  if the argument has the byval attribute AND the Function doesn't read 
    //  from memory continue to the next argument
    if (ArgumentConstants[i].second || AI->use_empty() ||
        AI->hasInAllocaAttr() || (AI->hasByValAttr() && !F.onlyReadsMemory()))
      continue;
  
    //  Grab the value of the Argument
    Value *V = ArgumentConstants[i].first;

    // If it is null then do something
    if (!V) V = UndefValue::get(AI->getType());

    //Here is the interprocedural constant prop happens 
    AI->replaceAllUsesWith(V);
    ++NumArgumentsProped;
    MadeChange = true;
  }
  return MadeChange;
}


// Check to see if this function returns one or more constants. If so, replace
// all callers that use those return values with the constant value. This will
// leave in the actual return values and instructions, but deadargelim will
// clean that up.
//
// Additionally if a function always returns one of its arguments directly,
// callers will be updated to use the value they pass in directly instead of
// using the return value.
bool IPCP::PropagateConstantReturn(Function &F) {
  if (F.getReturnType()->isVoidTy())
    return false; // No return value.

  // If this function could be overridden later in the link stage, we can't
  // propagate information about its results into callers.
  if (F.mayBeOverridden())
    return false;
    
  // Check to see if this function returns a constant.
  SmallVector<Value *,4> RetVals;
  StructType *STy = dyn_cast<StructType>(F.getReturnType());
  if (STy)
    for (unsigned i = 0, e = STy->getNumElements(); i < e; ++i) 
      RetVals.push_back(UndefValue::get(STy->getElementType(i)));
  else
    RetVals.push_back(UndefValue::get(F.getReturnType()));

  unsigned NumNonConstant = 0;
  for (Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB)
    if (ReturnInst *RI = dyn_cast<ReturnInst>(BB->getTerminator())) {
      for (unsigned i = 0, e = RetVals.size(); i != e; ++i) {
        // Already found conflicting return values?
        Value *RV = RetVals[i];
        if (!RV)
          continue;

        // Find the returned value
        Value *V;
        if (!STy)
          V = RI->getOperand(0);
        else
          V = FindInsertedValue(RI->getOperand(0), i);

        if (V) {
          // Ignore undefs, we can change them into anything
          if (isa<UndefValue>(V))
            continue;
          
          // Try to see if all the rets return the same constant or argument.
          if (isa<Constant>(V) || isa<Argument>(V)) {
            if (isa<UndefValue>(RV)) {
              // No value found yet? Try the current one.
              RetVals[i] = V;
              continue;
            }
            // Returning the same value? Good.
            if (RV == V)
              continue;
          }
        }
        // Different or no known return value? Don't propagate this return
        // value.
        RetVals[i] = nullptr;
        // All values non-constant? Stop looking.
        if (++NumNonConstant == RetVals.size())
          return false;
      }
    }

  // If we got here, the function returns at least one constant value.  Loop
  // over all users, replacing any uses of the return value with the returned
  // constant.
  bool MadeChange = false;
  for (Use &U : F.uses()) {
    CallSite CS(U.getUser());
    Instruction* Call = CS.getInstruction();

    // Not a call instruction or a call instruction that's not calling F
    // directly?
    if (!Call || !CS.isCallee(&U))
      continue;
    
    // Call result not used?
    if (Call->use_empty())
      continue;

    MadeChange = true;

    if (!STy) {
      Value* New = RetVals[0];
      if (Argument *A = dyn_cast<Argument>(New))
        // Was an argument returned? Then find the corresponding argument in
        // the call instruction and use that.
        New = CS.getArgument(A->getArgNo());
      Call->replaceAllUsesWith(New);
      continue;
    }

    for (auto I = Call->user_begin(), E = Call->user_end(); I != E;) {
      Instruction *Ins = cast<Instruction>(*I);

      // Increment now, so we can remove the use
      ++I;

      // Find the index of the retval to replace with
      int index = -1;
      if (ExtractValueInst *EV = dyn_cast<ExtractValueInst>(Ins))
        if (EV->hasIndices())
          index = *EV->idx_begin();

      // If this use uses a specific return value, and we have a replacement,
      // replace it.
      if (index != -1) {
        Value *New = RetVals[index];
        if (New) {
          if (Argument *A = dyn_cast<Argument>(New))
            // Was an argument returned? Then find the corresponding argument in
            // the call instruction and use that.
            New = CS.getArgument(A->getArgNo());
          Ins->replaceAllUsesWith(New);
          Ins->eraseFromParent();
        }
      }
    }
  }

  if (MadeChange) ++NumReturnValProped;
  return MadeChange;
}

