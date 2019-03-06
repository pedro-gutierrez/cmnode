export default (name, settings, app) => {
    const { encode, update } = app;
    return (encoders, enc, model) => {
        const { err, value } = encode(enc, model);
        if (err) {
            console.error("Encode error", err);
        } else {
            value.effect = name;
            setTimeout(() => { update(value); }, 0);
        }
    }
};
