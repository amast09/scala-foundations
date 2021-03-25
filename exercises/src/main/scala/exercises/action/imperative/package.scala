package exercises.action

package object imperative {

  // 1. Implement `retry`, a function which evaluates a block of code until either:
  // * It succeeds.
  // * Or the number of attempts is exhausted (when `maxAttempt` is 1).
  // For example,
  // var counter = 0
  // def exec(): String = {
  //   counter += 1
  //   require(counter >= 3, "Counter is too low")
  //   "Hello"
  // }
  // retry(maxAttempt = 5)( () => exec() ) == "Hello"
  // Returns "Hello" because `exec` fails twice and then succeeds when counter reaches 3.
  // retry(maxAttempt = 5){ () => throw new Exception("Boom!") }
  // Throws an exception because `block` fails every time it is evaluated
  // Note: `action: () => A` is a val function which takes 0 argument.
  //       You can create a 0-argument function using the syntax:
  //       * `() => { code }` (recommended syntax)
  //       * `def myMethod() = { code }` and then use eta-expansion to convert
  //          the def function `myMethod` into a val function.
  //       You can execute `action` using `action()`
  // Note: `maxAttempt` must be greater than 0, throw an exception if that's not the case.
  // Note: Tests are in the `exercises.action.imperative.ActionTest`
  def retry[A](maxAttempt: Int)(action: => A): A =
    ???

  // 2. Refactor `readSubscribeToMailingListRetry` using
  // `retry` and `readSubscribeToMailingList` (from `UserCreationExercises`).

  // 3. Refactor `readDateOfBirthRetry` using
  // `retry` and `readDateOfBirth` (from `UserCreationExercises`).

  //////////////////////////////////////////////
  //////////////////////////////////////////////
  //////////////////////////////////////////////
  //////////////////////////////////////////////
  //////////////////////////////////////////////
  //////////////////////////////////////////////
  //////////////////////////////////////////////
  //////////////////////////////////////////////

  // 4. Implement `onError` which executes `action`.
  // If an error occurs, it calls the `callBack` function with the error and then rethrow it.
  // For example,
  // onError(() => 1, _ => println("Hello"))
  // print nothing and return 1 because action succeeds.
  // But,
  // onError(() => throw new Exception("Boom"), _ => println("Hello"))
  // print "Hello" and then rethrow the "Boom" exception.
  // Note: What should happen if the `callback` function fails?
  def onError[A](action: => A, callback: Throwable => Any): A =
    ???

  // 5. Refactor `readSubscribeToMailingList` and `readDateOfBirth` using `onError`

}