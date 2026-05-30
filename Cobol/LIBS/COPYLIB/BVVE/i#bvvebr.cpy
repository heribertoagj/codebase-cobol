           SKIP2
      *------------------------------------------------------------*
      * RECSZ=220      F2D-CADASTRO DE DETALHES    -INC I#BVVEBR   *
      * INC I#DESCAR (ANTIGA)  ==> TAMANHO ==> 200                 *
      * INC I#DECEAR (NOVA)    ==> TAMANHO ==> 220 - COPIA I#DECEBR*
      *------------------------------------------------------------*
           SKIP1
       01  DWORKF2D.
           02  DCHAVE2D.
               03  DAGENCIA            PIC 9(05).
               03  DCONTCOR.
                   04  DCONTA          PIC 9(07).
                   04  DDIGITO         PIC X.
               03  DCARTEIR            PIC X.
               03  DNOSNUME.
                   04  DNNCODAT        PIC 9(03).
                   04  DNNUMERO        PIC 9(06).
               03  DNOSNUME-R  REDEFINES  DNOSNUME   PIC 9(09).
           02  DVENCTO                 PIC 9(08).
           02  DVALOR                  PIC 9(11)V99    COMP-3.
           02  DRAMO                   PIC X.
           02  DDEPOSIT                PIC 9(07)       COMP-3.
           02  DAUTORIZ                PIC 9(02).
           02  DSEUNUME                PIC X(15).
           02  DDUPNP                  PIC X.
           02  DCEV                    PIC 9.
           02  DCDA                    PIC X.
           02  DLP                     PIC 9.
           02  DCOMISS                 PIC 9.
           02  DJUCOTAX                PIC 9(11)V99    COMP-3.
           02  DPRACAS                 PIC X.
           02  DNOMESAC                PIC X(40).
           02  REGPENDE.
               03  DENDESAC            PIC X(44).
               03  DDTDESC             PIC 9(09)       COMP-3.
               03  DMOTIVO             PIC 9.
               03  DCTP                PIC X.
               03  DCCB                PIC X.
           02  REGTBAIX                REDEFINES       REGPENDE.
               03  FILLER              PIC X(04).
               03  DDTBAIXA            PIC 9(09)       COMP-3.
               03  DDTPGTO             PIC 9(09)       COMP-3.
               03  DAGCOBRA            PIC 9(05)       COMP-3.
               03  DVALORCB            PIC 9(11)V99    COMP-3.
               03  DJUMORAC            PIC 9(07)V99    COMP-3.
               03  DISMORAC            PIC 9(07)V99    COMP-3.
               03  DDTTRANS            PIC 9(09)       COMP-3.
               03  DDTCONTB            PIC 9(09)       COMP-3.
               03  FILLER              PIC X(08).
           02  DCGCSAC                 PIC 9(15)       COMP-3.
           02  DCEPSAC                 PIC 9(05)       COMP-3.
           02  DTAXAS.
               03  DTAXA               PIC 9(03)V99    COMP-3.
               03  DTXCOR              PIC 9(03)V99    COMP-3.
               03  DEN                 PIC X.
           02  DMORDIA                 PIC 9(11)V99    COMP-3.
      *----A REDEFINCAO JURMORA CONTEM JUROS MORA CONTABILIZADO PARA
      *----  MIGRACAO BANEB / BEA / MERCANTIL
           02  JURMORA  REDEFINES DMORDIA
                                       PIC 9(11)V99    COMP-3.
           02  DISOF                   PIC 9(09)V99    COMP-3.
           02  DNOVOCEP.
               03  DCEPNOVO            PIC 9(05)       COMP-3.
               03  DSUFIXO             PIC X(03).
           02  DVALTIT                 PIC 9(11)V99    COMP-3.
           02  DVALTIT-R               REDEFINES       DVALTIT.
               03  DBANCOCOBR          PIC 9(03)       COMP-3.
               03  DVALBONIF           PIC 9(07)V99    COMP-3.
           02  DIDENURV                PIC X(01).
           02  DDTEMIS                 PIC 9(09)       COMP-3.
           02  DVALTARIF               PIC 9(03)V99    COMP-3.
           02  DPRODUTO                PIC 9(02).
           SKIP3
