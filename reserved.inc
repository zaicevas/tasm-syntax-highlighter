; ALL reserved words that will be scanned and (possibly) highlighted

_db db "db"
_dw db "dw"
_enum db "enum"
_struc db "struc"
_union db "union"
_record db "record"

_ax db "ax"
_ah db "ah"
_al db "al"
_bx db "bx"
_bl db "bl"
_bh db "bh"
_cx db "cx"
_cl db "cl"
_ch db "ch"
_dx db "dx"
_dh db "dh"
_dl db "dl"
_cs db "cs"
_ds db "ds"
_ss db "ss"
_es db "es"
_si db "si"
_di db "di"
_bp db "bp"
_sp db "sp"

_aaa db "aaa"
_aad db "aad"
_aam db "aam"
_aas db "aas"
_adc db "adc"
_add db "add"
_and db "and"
_call db "call"
_cbw db "cbw"
_clc db "clc"
_cld db "cld"
_cli db "cli"
_cmc db "cmc"
_cmp db "cmp"
_cmpsb db "cmpsb"
_cmpsw db "cmpsw"
_cwd db "cwd"
_daa db "daa"
_das db "das"
_dec db "dec"
_div db "div"
_hlt db "hlt"
_idiv db "idiv"
_imul db "imul"
_in db "in"
_inc db "inc"
_int db "int"
_into db "into"
_iret db "iret"
_ja db "ja"
_jae db "jae"
_jb db "jb"
_jbe db "jbe"
_jc db "jc"
_jcxz db "jcxz"
_je db "je"
_jg db "jg"
_jge db "jge"
_jl db "jl"
_jle db "jle"
_jmp db "jmp"
_jna db "jna"
_jnae db "jnae"
_jnb db "jnb"
_jnbe db "jnbe"
_jnc db "jnc"
_jne db "jne"
_jng db "jng"
_jnge db "jnge"
_jnl db "jnl"
_jnle db "jnle"
_jno db "jno"
_jnp db "jnp"
_jns db "jns"
_jnz db "jnz"
_jo db "jo"
_jp db "jp"
_jpe db "jpe"
_jpo db "jpo"
_js db "js"
_jz db "jz"
_lahf db "lahf"
_lds db "lds"
_lea db "lea"
_les db "les"
_lodsb db "lodsb"
_lodsw db "lodsw"
_loop db "loop"
_loope db "loope"
_loopne db "loopne"
_loopnz db "loopnz"
_loopz db "loopz"
_mov db "mov"
_movsb db "movsb"
_movsw db "movsw"
_mul db "mul"
_neg db "neg"
_nop db "nop"
_not db "not"
_or db "or"
_out db "out"
_pop db "pop"
_popa db "popa"
_popf db "popf"
_push db "push"
_pusha db "pusha"
_pushf db "pushf"
_rcl db "rcl"
_rcr db "rcr"
_rep db "rep"
_repe db "repe"
_repne db "repne"
_repnz db "repnz"
_repz db "repz"
_ret db "ret"
_retf db "retf"
_rol db "rol"
_ror db "ror"
_sahf db "sahf"
_sal db "sal"
_sar db "sar"
_sbb db "sbb"
_scasb db "scasb"
_scasw db "scasw"
_shl db "shl"
_shr db "shr"
_stc db "stc"
_std db "std"
_sti db "sti"
_stosb db "stosb"
_stosw db "stosw"
_sub db "sub"
_test db "test"
_xchg db "xchg"
_xlatb db "xlatb"
_xor db "xor"

_ptr db "ptr"

_strings LABEL reserved
reserved <2, offset _db, datatype>
reserved <2, offset _dw, datatype>
reserved <4, offset _enum, datatype>
reserved <5, offset _struc, datatype>
reserved <5, offset _union, datatype>
reserved <6, offset _record, datatype>

reserved <2, offset _ax, register>
reserved <2, offset _ah, register>
reserved <2, offset _al, register>
reserved <2, offset _bx, register>
reserved <2, offset _bl, register>
reserved <2, offset _bh, register>
reserved <2, offset _cx, register>
reserved <2, offset _cl, register>
reserved <2, offset _ch, register>
reserved <2, offset _dx, register>
reserved <2, offset _dh, register>
reserved <2, offset _dl, register>
reserved <2, offset _cs, register>
reserved <2, offset _ds, register>
reserved <2, offset _ss, register>
reserved <2, offset _es, register>
reserved <2, offset _si, register>
reserved <2, offset _di, register>
reserved <2, offset _bp, register>
reserved <2, offset _sp, register>

reserved <3, offset _aaa, instruction>
reserved <3, offset _aad, instruction>
reserved <3, offset _aam, instruction>
reserved <3, offset _aas, instruction>
reserved <3, offset _adc, instruction>
reserved <3, offset _add, instruction>
reserved <3, offset _and, instruction>
reserved <4, offset _call, instruction>
reserved <3, offset _cbw, instruction>
reserved <3, offset _clc, instruction>
reserved <3, offset _cld, instruction>
reserved <3, offset _cli, instruction>
reserved <3, offset _cmc, instruction>
reserved <3, offset _cmp, instruction>
reserved <5, offset _cmpsb, instruction>
reserved <5, offset _cmpsw, instruction>
reserved <3, offset _cwd, instruction>
reserved <3, offset _daa, instruction>
reserved <3, offset _das, instruction>
reserved <3, offset _dec, instruction>
reserved <3, offset _div, instruction>
reserved <3, offset _hlt, instruction>
reserved <4, offset _idiv, instruction>
reserved <4, offset _imul, instruction>
reserved <2, offset _in, instruction>
reserved <3, offset _inc, instruction>
reserved <3, offset _int, instruction>
reserved <4, offset _into, instruction>
reserved <4, offset _iret, instruction>
reserved <2, offset _ja, instruction>
reserved <3, offset _jae, instruction>
reserved <2, offset _jb, instruction>
reserved <3, offset _jbe, instruction>
reserved <2, offset _jc, instruction>
reserved <4, offset _jcxz, instruction>
reserved <2, offset _je, instruction>
reserved <2, offset _jg, instruction>
reserved <3, offset _jge, instruction>
reserved <2, offset _jl, instruction>
reserved <3, offset _jle, instruction>
reserved <3, offset _jmp, instruction>
reserved <3, offset _jna, instruction>
reserved <4, offset _jnae, instruction>
reserved <3, offset _jnb, instruction>
reserved <4, offset _jnbe, instruction>
reserved <3, offset _jnc, instruction>
reserved <3, offset _jne, instruction>
reserved <3, offset _jng, instruction>
reserved <4, offset _jnge, instruction>
reserved <3, offset _jnl, instruction>
reserved <4, offset _jnle, instruction>
reserved <3, offset _jno, instruction>
reserved <3, offset _jnp, instruction>
reserved <3, offset _jns, instruction>
reserved <3, offset _jnz, instruction>
reserved <2, offset _jo, instruction>
reserved <2, offset _jp, instruction>
reserved <3, offset _jpe, instruction>
reserved <3, offset _jpo, instruction>
reserved <2, offset _js, instruction>
reserved <2, offset _jz, instruction>
reserved <4, offset _lahf, instruction>
reserved <3, offset _lds, instruction>
reserved <3, offset _lea, instruction>
reserved <3, offset _les, instruction>
reserved <5, offset _lodsb, instruction>
reserved <5, offset _lodsw, instruction>
reserved <4, offset _loop, instruction>
reserved <5, offset _loope, instruction>
reserved <6, offset _loopne, instruction>
reserved <6, offset _loopnz, instruction>
reserved <5, offset _loopz, instruction>
reserved <3, offset _mov, instruction>
reserved <5, offset _movsb, instruction>
reserved <5, offset _movsw, instruction>
reserved <3, offset _mul, instruction>
reserved <3, offset _neg, instruction>
reserved <3, offset _nop, instruction>
reserved <3, offset _not, instruction>
reserved <2, offset _or, instruction>
reserved <3, offset _out, instruction>
reserved <3, offset _pop, instruction>
reserved <4, offset _popa, instruction>
reserved <4, offset _popf, instruction>
reserved <4, offset _push, instruction>
reserved <5, offset _pusha, instruction>
reserved <5, offset _pushf, instruction>
reserved <3, offset _rcl, instruction>
reserved <3, offset _rcr, instruction>
reserved <3, offset _rep, instruction>
reserved <4, offset _repe, instruction>
reserved <5, offset _repne, instruction>
reserved <5, offset _repnz, instruction>
reserved <4, offset _repz, instruction>
reserved <3, offset _ret, instruction>
reserved <4, offset _retf, instruction>
reserved <3, offset _rol, instruction>
reserved <3, offset _ror, instruction>
reserved <4, offset _sahf, instruction>
reserved <3, offset _sal, instruction>
reserved <3, offset _sar, instruction>
reserved <3, offset _sbb, instruction>
reserved <5, offset _scasb, instruction>
reserved <5, offset _scasw, instruction>
reserved <3, offset _shl, instruction>
reserved <3, offset _shr, instruction>
reserved <3, offset _stc, instruction>
reserved <3, offset _std, instruction>
reserved <3, offset _sti, instruction>
reserved <5, offset _stosb, instruction>
reserved <5, offset _stosw, instruction>
reserved <3, offset _sub, instruction>
reserved <4, offset _test, instruction>
reserved <4, offset _xchg, instruction>
reserved <5, offset _xlatb, instruction>
reserved <3, offset _xor, instruction>

reserved <3, offset _ptr, pointr>
