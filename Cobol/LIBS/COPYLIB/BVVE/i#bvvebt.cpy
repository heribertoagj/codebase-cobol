           SKIP2
      *------------------------------------------------------------*
      * RECSZ=460      F6-ENTRADAS                 -INC I#BVVEBT   *
      * INC I#DESCAT (ANTIGA) - TAMANHO = 450                      *
      * INC I#DECEP2 (NOVA)   - TAMANHO = 480 - COPIA DA I#DECEBT  *
      *------------------------------------------------------------*
           SKIP1
       01  WORKF6EN.
           02  EAGENCIA                PIC 9(05)       COMP-3.
           02  ECONTA                  PIC 9(07)       COMP-3.
           02  EDIGITO                 PIC X.
           02  FILLER                  PIC X.
           02  ECART                   PIC X.
           02  ENOSSONU                PIC 9(11)       COMP-3.
           02  EDEPOSIT.
               03  EPREFDEP            PIC 9(03)       COMP-3.
               03  EAGDEPOS            PIC 9(05)       COMP-3.
           02  ESEUNUME                PIC X(15).
           02  ERAMODIG                PIC X.
           02  EVALOR                  PIC 9(11)V99    COMP-3.
           02  EAUTORIZ                PIC 9(02).
           02  EDTVENC                 PIC 9(09)       COMP-3.
           02  EDTDESC                 PIC 9(09)       COMP-3.
           02  EDUPNP                  PIC X.
           02  ECDCOMIS                PIC 9.
           02  EPESSOA                 PIC X.
           02  ESACADO                 PIC X(40).
           02  EENDSAC                 PIC X(40).
           02  ECODCENT                PIC 9.
           02  ENOMAGCE                PIC X(20).
           02  ENOMAGDE                PIC X(28).
           02  ENUMSEQ                 PIC 9(09)       COMP-3.
           02  ETXAPLI                 PIC 9(03)V99    COMP-3.
           02  ETXCORR                 PIC 9(03)V99    COMP-3.
           02  ENORMESP                PIC X.
           02  ENOMEPRO                PIC X(40).
           02  ECGCPROP                PIC 9(15)       COMP-3.
           02  ENUMAUT                 PIC 9(05)       COMP-3.
           02  EBCORRES                PIC X.
           02  ESUBCENT                PIC 9(05)       COMP-3.
           02  ECOMISAL                PIC X.
           02  ECDA                    PIC X.
           02  ECGCSAC.
               03  ECGCNUM             PIC 9(11)       COMP-3.
               03  ECGCFIL             PIC 9(05)       COMP-3.
               03  ECGCCTR             PIC 99.
           02  ECD                     PIC 9(05)       COMP-3.
           02  ECAMBIO.
               03  ECBNUMER            PIC 9(09)       COMP-3.
               03  ECBCOMIS            PIC 9(05)       COMP-3.
           02  ECODSEG                 PIC 9(02).
           02  ERAMO                   PIC X.
           02  EJUCOMI                 PIC 9(11)V99    COMP-3.
           02  EFECHACB                PIC 9(11)V99    COMP-3.
           02  EIOF                    PIC 9(09)V99    COMP-3.
           02  ETEMPIOF                PIC X.
           02  ETXCOB                  PIC 9(07)V99    COMP-3.
           02  EDESPORT                PIC 9(07)V99    COMP-3.
           02  EENDAGDP                PIC X(40).
           02  ECOMISSX                PIC 9(11)V99    COMP-3.
           02  EMORADIA                PIC 9(11)V99    COMP-3.
           02  ETXESPCO                PIC 9.
           02  ECDRESPO                PIC 9(07)       COMP-3.
           02  ECEPNOVO.
               03  ECEPSAC             PIC 9(05)       COMP-3.
               03  ESUFIXO             PIC X(03).
           02  ESEQACE                 PIC 9(11)       COMP-3.
           02  ERZPROPO                PIC 9(05)       COMP-3.
           02  ETXBORDE                PIC 9(07)V99    COMP-3.
           02  ERZDEB                  PIC 9(05)       COMP-3.
           02  ECIDADE                 PIC X(15).
           02  EESTADO                 PIC X(02).
           02  ENUDIAS                 PIC 9(05)       COMP-3.
           02  EVRDEFLA                PIC 9(11)V99    COMP-3.
           02  EPRAZO                  PIC X(01).
           02  EVLRURV                 PIC 9(11)V99    COMP-3.
           02  ECEP-DEP                PIC 9(05)       COMP-3.
           02  ESUF-DEP                PIC X(03).
           02  ECODCRS                 PIC 9(05)       COMP-3.
           02  EBONIF-ANTECIP          PIC 9(07)V99    COMP-3.
           02  EDATA-EMISSAO           PIC 9(09)       COMP-3.
           02  EESPECIE                PIC X(02).
           02  EDATA-CONTAB            PIC 9(05)       COMP-3.
      *-------DATA CONTABILIZACAO IOF - FORMATO JULIANO
           02  ECONTROLE               PIC 9(09)       COMP-3.
           02  EPRODUTO                PIC 9(02).
           02  FILLER                  PIC X(19).
           SKIP3
