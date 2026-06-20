      ***********************************************************
      **                                                       **
      **                       I#MESUM4                        **
      **    AREA DE PARAMETROS DO MODULO MESU9014              **
      **                                                       **
      **               DATA DE ATUALIZACAO = 21/06/2002        **
      **                                                       **
      ***********************************************************
       01  COMU-DADOS-9014.
           05   COMU-VERSAO                      PIC X(006).
           05   COMU-MENSAGEM.
             10 COMU-COD-MENSAGEM                PIC 9(003).
             10 COMU-HIFEN                       PIC X(001).
             10 COMU-TEXTO-MSG                   PIC X(079).
           05   COMU-DADOS-ENTRADA.
             10 COMU-COD-EMPR                    PIC  9(005).
             10 COMU-COD-DEPDC                   PIC  9(005).
           05   COMU-DADOS-SAIDA.
             10 COMU-NOME-DEPDC                  PIC  X(050).
             10 COMU-COD-DIR                     PIC  9(003).
             10 COMU-NOME-DIR                    PIC  X(050).
             10 COMU-JUNC-DIR                    PIC  9(005).
             10 COMU-COD-REGIO-DIR               PIC  9(003).
             10 COMU-COD-DIS                     PIC  9(005).
             10 COMU-COD-GER-RESP                PIC  9(009).
             10 COMU-NOME-GER-RESP               PIC  X(040).
