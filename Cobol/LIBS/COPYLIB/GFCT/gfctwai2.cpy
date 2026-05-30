      *****************************************************************
      * NOME DA INC - GFCTWAI2                                        *
      * DESCRICAO   - MODULO ADESAO PACOTES CONTAS PAN - ENTRADA      *
      * TAMANHO     - 80                                              *
      * DATA        - 21.08.2009                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 45062345       *
      *****************************************************************
      * REDEFINIDO SEGUNDA AREA PARA CELULAR 09 DIGITOS               *
      * A SER USADO NA VERSAO 003 DE TRANSACAO DAS CONTAS PAN.        *
      *****************************************************************
      *
       01  GFCTWAI2-ENTRADA.
           03  GFCTWAI2-LL                            PIC S9(004) COMP.
           03  GFCTWAI2-ZZ                            PIC S9(004) COMP.
           03  GFCTWAI2-TRANSACAO                     PIC  X(009).
           03  GFCTWAI2-FUNCAO                        PIC  X(008).
           03  GFCTWAI2-DADOS-ENTRADA.
               05  GFCTWAI2-VERSAO                    PIC  9(003).
               05  GFCTWAI2-AGENCIA                   PIC  9(005).
               05  GFCTWAI2-CONTA                     PIC  9(007).
               05  GFCTWAI2-DIGITO-CONTA              PIC  9(001).
               05  GFCTWAI2-PAB                       PIC  9(005).
               05  GFCTWAI2-PACOTE                    PIC  9(005).
               05  GFCTWAI2-SEGMENTO-CLIE             PIC  9(003).
               05  GFCTWAI2-DT-ABERTURA-CONTA         PIC  9(008).
               05  GFCTWAI2-DDD.
                   10 GFCTWAI2-DDD-N                  PIC  9(003).
               05  GFCTWAI2-NUMERO.
                   10 GFCTWAI2-NUMERO-N               PIC  9(008).
               05  GFCTWAI2-OPERADORA.
                   10 GFCTWAI2-OPERADORA-N            PIC  9(009).
               05  FILLER                             PIC  X(002).
       01  WAI2B-ENTRADA REDEFINES GFCTWAI2-ENTRADA.
           03  WAI2B-LL                            PIC S9(004) COMP.
           03  WAI2B-ZZ                            PIC S9(004) COMP.
           03  WAI2B-TRANSACAO                     PIC  X(009).
           03  WAI2B-FUNCAO                        PIC  X(008).
           03  WAI2B-DADOS-ENTRADA.
               05  WAI2B-VERSAO                    PIC  9(003).
               05  WAI2B-AGENCIA                   PIC  9(005).
               05  WAI2B-CONTA                     PIC  9(007).
               05  WAI2B-DIGITO-CONTA              PIC  9(001).
               05  WAI2B-PAB                       PIC  9(005).
               05  WAI2B-PACOTE                    PIC  9(005).
               05  WAI2B-SEGMENTO-CLIE             PIC  9(003).
               05  WAI2B-DT-ABERTURA-CONTA         PIC  9(008).
               05  WAI2B-DDD.
                   10 WAI2B-DDD-N                  PIC  9(003).
               05  WAI2B-NUMERO.
                   10 WAI2B-NUMERO-N               PIC  9(009).
               05  WAI2B-OPERADORA.
                   10 WAI2B-OPERADORA-N            PIC  9(009).
               05  FILLER                          PIC  X(001).

      *
