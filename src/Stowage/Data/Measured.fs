namespace Stowage

/// IMeasured indicates a value can be summarized or measured by
/// another value of type M. In general, the summary should be
/// much smaller than the value being summarized!
type IMeasured<'M> =
    abstract member Measure : 'M

