let ste = ReasonReact.string;

module CreateSignupMutation = [%graphql
  {|
  mutation signup($email: String!, $recaptcha: String!) {
    signup(email: $email, recaptcha: $recaptcha) {
      errors { key message }
    }
  }
|}
];

module CreateSignup = ReasonApollo.CreateMutation(CreateSignupMutation);

module SignupForm = {
  type field =
    | Email
    | Recaptcha;
  type value = string;
  type state = {
    email: string,
    recaptcha: string,
  };
  type message = string;
  let get = (field, state) =>
    switch (field) {
    | Email => state.email
    | Recaptcha => state.recaptcha
    };
  let update = ((field, value), state) =>
    switch (field, value) {
    | (Email, value) => {...state, email: value}
    | (Recaptcha, value) => {...state, recaptcha: value}
    };
  /* fieldStrToField enables rendering ReformalityServerErrors and is not required by ReFormality itself */
  let fieldStrToField = fieldStr =>
    switch (fieldStr) {
    | "email" => Email
    | _ => Email /* Unsure what to do here, throw? */
    };
  let valueEmpty = Formality.emptyString;

  module Validators =
    Formality.MakeValidators({
      type t = field;
    });

  type validators = Validators.t(Formality.validator(field, value, state, message));
  let validators =
    Formality.(
      Validators.empty
      |> Validators.add(
           Email,
           {
             strategy: Strategy.OnFirstSuccessOrFirstBlur,
             dependents: None,
             validate: (value, _state) => {
               /* TODO: following regex matches Elixir version but should guard against consecutive dots (.) */
               let emailRegex = [%bs.re {|/^[\w+.-]+@[a-z\d.-]+\.[a-z]+$/i|}];
               switch (value) {
               | "" => Invalid("Email address is required")
               | _ when !(emailRegex |> Js.Re.test(value)) => Invalid("Invalid email address")
               | _ => Valid
               };
             },
           },
         )
      |> Validators.add(
           Recaptcha,
           {
             strategy: Strategy.OnSubmit,
             dependents: None,
             validate: (value, _state) =>
               switch (Env.env, value) {
               | ("dev", _) => Valid
               | ("test", _) => Valid
               | (_, "") => Invalid("Recaptcha needs solving")
               | (_, _) => Valid
               },
           },
         )
    );
};

module SignupFormContainer = Formality.Make(SignupForm);

let component = ReasonReact.statelessComponent(__MODULE__);

let logChange = value => Js.log2("captcha value:", value);

let recaptchaSitekey =
  switch (Env.env) {
  | "test" => "6LeIxAcTAAAAAJcZVRqyHh71UMIEGNQ_MXjiZKhI"
  | _ => "6LecRi4UAAAAAMk65p0gwBQfJX_FOhvq_3et8yHV"
  };

let make = _children => {
  ...component,
  render: _self =>
    <CreateSignup>
      ...{
           (mutation, _) =>
             /* NOTE: Apollo Errors (Network Errors, "failed to fetch", or if the query is malformed
                according to the server (400 error)) only appear up here at the top not in the mutation Promise */
             <SignupFormContainer
               initialState={email: "", recaptcha: ""}
               onSubmit={
                 (state, {notifyOnSuccess, notifyOnFailure}) => {
                   let newSignup =
                     CreateSignupMutation.make(
                       ~email=state.email,
                       ~recaptcha=state.recaptcha,
                       (),
                     );
                   /*
                     On Apollo Error, reset the form and flash Unknown error message
                     On Data Error(s), pipe the server errors into notifyOnFailure so they can be rendered
                     On Data success, save a flash message to a cookie? then redirect to Home -- do we need to notifyOnSuccess also?
                    */
                   mutation(~variables=newSignup##variables, ())
                   |> Js.Promise.then_(rawResult => {
                        let parsedResult = CreateSignup.convertJsInputToReason(rawResult).result;
                        switch (parsedResult) {
                        | NotCalled
                        | Loading => ()
                        | Error(err) => Js.log2("Error from GraphQL", err)
                        | Data(data) =>
                          /* Js.log(data##signup##errors); */
                          switch (data##signup##errors) {
                          | [||] =>
                            TCookie.setFlashMsg(
                              ~type_="is-info",
                              ~message=
                                "Thank you for signing up for Zenterra.  Please check your email for next steps.",
                            );
                            notifyOnSuccess(None);
                            Webapi.Dom.Location.setHref(Webapi.Dom.location, "/");
                          | svrErrors =>
                            Js.log2("number of server errors", Belt.Array.length(svrErrors));
                            ReformalityServerErrors.svrErrorsToReformality(
                              svrErrors,
                              SignupForm.fieldStrToField,
                            )
                            |> notifyOnFailure(_, None);
                          }
                        };
                        Js.Promise.resolve();
                      })
                   |> Js.Promise.catch(err => {
                        /* Unsure what cases we arrive here */
                        Js.log2("Error in Promise", err);
                        Js.Promise.resolve();
                      })
                   |> ignore;
                 }
               }>
               ...{
                    form =>
                      <Fragment>
                        <ReformalityServerErrors
                          formStatus={form.status}
                          onDismiss={
                            _event => {
                              Js.log("removing last submission result");
                              form.dismissSubmissionResult();
                            }
                          }
                        />
                        <form
                          id="signup_form" onSubmit={form.submit |> Formality.Dom.preventDefault}>
                          /* TODO: Set to disabled if the form status is SubmissionFailed also */

                            <TInputField
                              formField="signup_email"
                              inputType="email"
                              disabled={form.submitting}
                              value={form.state.email}
                              label="Business email"
                              placeholder="name@company.com"
                              message=?{
                                SignupForm.Email |> form.results |> Utils.reformalityResultToOptStr
                              }
                              icon="envelope"
                              onChange={
                                event =>
                                  event
                                  |> Formality.Dom.toValueOnChange
                                  |> form.change(SignupForm.Email)
                              }
                              onBlur={
                                event =>
                                  event
                                  |> Formality.Dom.toValueOnBlur
                                  |> form.blur(SignupForm.Email)
                              }
                              autofocus=true
                            />
                            {
                              if (Env.env == "prod") {
                                <ReactGoogleRecaptcha
                                  ref_="recaptcha"
                                  sitekey=recaptchaSitekey
                                  onChange={
                                    recaptchaResult => {
                                      Js.log2("recaptcha result:", recaptchaResult);
                                      form.change(SignupForm.Recaptcha, recaptchaResult);
                                    }
                                  }
                                />;
                              } else {
                                ste("Recaptcha shows here in production");
                              }
                            }
                            <TButton
                              id="submit_button"
                              disabled={form.submitting}
                              label="Submit"
                              labelWhenDisabled="Submitting..."
                            />
                          </form>
                      </Fragment>
                      /* <div
                         className="field"
                         style=(ReactDOMRe.Style.make(~marginTop="24px", ()))>
                         /* TODO: set to disabled if the form status is invalid also */

                           <button
                             id="submit_button"
                             className="button is-primary is-fullwidth"
                             style=(ReactDOMRe.Style.make(~display="block", ()))
                             disabled=form.submitting>
                             ((form.submitting ? "Submitting..." : "Submit") |> ste)
                           </button>
                         </div> */
                  }
             </SignupFormContainer>
         }
    </CreateSignup>,
};
