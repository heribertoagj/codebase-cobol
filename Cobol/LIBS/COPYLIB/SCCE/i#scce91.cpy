      ***===========================================================***
      *    NOME DA INC =  I#SCCE91                                    *
      *                                                               *
      *    DATA        =  JUNHO/2000                                  *
      *    RESPONSAVEL =  SERGIO G. GARCIA - STEFANINI                *
      *    GRUPO       =  83                                          *
      *                                                               *
      *    AREAS DE COMUNICAÇAO:                                      *
      *    --------------------                                       *
      * . ACESSO A SUBROTINA DE VERIFICACAO DE CONTA ENCERRADA        *
      *                                                               *
      * . PARAMETROS A SEREM USADOS PELA POLL7100 PARA MODULOS DB2,   *
      *     EM PROGRAMAS ON-LINE                                      *
      ***===========================================================***

      ***===========================================================***
      *    AREA DE COMUNICACAO                                        *
      *    ACESSO A SUBROTINA DE VERIFICACAO DE CONTA ENCERRADA       *
      *                                                               *
      *    TAMANHO     =  90                                          *
      ***===========================================================***

       01  LKW-AREA.
           03  LKW-DENCRR              PIC  X(10).
           03  LKW-CMOTVO-ENCRR-CTA    PIC  X(01).
           03  LKW-CJUNC-DEPC          PIC  9(05).
           03  LKW-CTA-CORR            PIC  9(07).
           03  LKW-CSERVC-ESPEC        PIC  9(02).
           03  LKW-QT-DIA-EXCLUSAO-CO  PIC  9(03).
           03  LKW-CD-RETORNO          PIC  9(02).
           03  LKW-MENSAGEM            PIC  X(30).
           03  LKW-PROGRAMA            PIC  X(08).
           03  FILLER                  PIC  X(22).

      *   OBSERVACAO:
      *   DENCRR
      *     SE IGUAL      A 33331231 E' PASSIVEL DE ENCERRAMENTO
      *     SE DIFERENTE DE 33331231 E' ENCERRADA E CONTEM A
      *   CSERVC-ESPEC                  DATA DO ENCERRAMENTO
      *     IGUAL A '06'
      *
      *---------------------------------------------------------------*
      *   RETORNO DO ACESSO
      *---------------------------------------------------------------*
      *
      *   CD-RETORNO
      *   .00 ->     'TERMINO NORMAL'
      *   .01 ->     'CONTA CORRENTE NAO RECADASTRADA'
      *   .02 ->     'POUPANCA NAO RECADASTRADA'
      *   .03 ->     'CONTA ENCERRADA'
      *   .04 ->     'CONTA PASSIVEL DE ENCERRAMENTO'
      *   .05 ->     'AGENCIA E/OU CONTA NAO NUMERICA'
      *   .99 ->     'ERRO ...'
      ***===========================================================***

      ***===========================================================***
      * AREA DE COMUNICACAO DE MODULOS DB2 COM PROGRAMAS ON-LINE      *
      * ONDE SAO INFORMADOS OS PARAMETROS USADOS PELA POLL7100        *
      ***===========================================================***
      *---------------------------------------------------------------*
      *    P C B ' S
      *---------------------------------------------------------------*
       01  LNK-PCB-IO.
           03  PCB-IO-TERM.
               05  PCB-IO-TERM1        PIC X(01).
               05  PCB-IO-TERM2        PIC X(07).
           03  FILLER                  PIC X(02).
           03  PCB-IO-SC               PIC X(02).
           03  FILLER                  PIC X(12).
           03  PCB-IO-MOD              PIC X(08).
           03  FILLER                  PIC X(08).
       01  LNK-PCB-ALT.
           03  PCB-ALT-TERM            PIC X(08).
           03  FILLER                  PIC X(02).
           03  PCB-ALT-SC              PIC X(02).
       01  LNK-ID-CD-USUARIO-SENHA     PIC X(07).
       01  LNK-CODFUNC-SENHA REDEFINES
           LNK-ID-CD-USUARIO-SENHA     PIC 9(07).
       01  FILLER REDEFINES LNK-ID-CD-USUARIO-SENHA.
           03  LNK-ID-USUARIO-SENHA    PIC X(01).
           03  LNK-CD-USUARIO-SENHA    PIC 9(06).
       01  LNK-ID-CD-DEPTO-SENHA       PIC X(06).
      *
      ***===========================================================***
