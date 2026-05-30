      ***********************************************************
      **                                                       **
      **                       I#MESUM6                        **
      **    AREA DE PARAMETROS DO MODULO MESU9015              **
      **                                                       **
      **               DATA DE ATUALIZACAO = XX/09/2002        **
      **                                                       **
      ***********************************************************
       01  COMU-PARM.
           03  COMU-ENTRADAS.
               05  COMU-NOME-PGM            PIC X(08).
               05  COMU-COD-RETORNO         PIC 9(02).
               05  COMU-BANCO               PIC 9(03).
               05  COMU-DEPDC               PIC 9(05).
           03  COMU-SAIDAS.
               05  COMU-DG-DEPDC            PIC X(01).
               05  COMU-NOME-DEPDC          PIC X(40).
               05  COMU-END-DEPDC           PIC X(30).
               05  COMU-DIRETORIA-REG       PIC 9(05).
               05  COMU-DIRETORIA-NOME      PIC X(40).
               05  COMU-NOME-BANCO          PIC X(40).
               05  COMU-POLO-SERVICO        PIC 9(05).
               05  COMU-MUNIC-DEPDC         PIC X(25).
               05  COMU-UF                  PIC X(02).
               05  COMU-CEP                 PIC 9(05).
               05  COMU-CEP-COMPL           PIC 9(03).
               05  COMU-INDIC-DEPDC-AT      PIC X(01).
               05  COMU-NATUREZA-DEPDC      PIC X(01).
               05  COMU-COD-DISTR-CORR      PIC 9(05).
               05  COMU-DENCRR-DEPDC-BDSCO  PIC X(10).
               05  COMU-FILLER              PIC X(35).
       01  WRK-POOL7100                     PIC X(107).
       01  WRK-SQLCA                        PIC X(136).
