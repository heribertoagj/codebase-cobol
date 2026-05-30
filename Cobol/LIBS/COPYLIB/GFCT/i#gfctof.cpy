      *****************************************************************
      * NOME DA INC - I#GFCTOF                                        *
      * DESCRICAO   - EXCECAO DE ADESAO COMPULSORIA - CONS DO CANCEL  *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTOF-LL   - 1010                                            *
      * DATA        - ABRIL/2006                                      *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTOF-ENTRADA.
           03 GFCTOF-LL                            PIC S9(004) COMP.
           03 GFCTOF-ZZ                            PIC S9(004) COMP.
           03 GFCTOF-TRANSACAO                     PIC  X(009).
           03 GFCTOF-FUNCAO                        PIC  X(008).
           03 GFCTOF-DADOS-RST.
              05 GFCTOF-FUNC-BDSCO                 PIC  X(007).
              05 GFCTOF-TERMINAL                   PIC  X(008).
              05 GFCTOF-FIM                        PIC  X(001).
              05 GFCTOF-FILLER                     PIC  X(611).
           03 GFCTOF-FIXO.
              05 GFCTOF-CAGENCIA-EXCEC             PIC  9(005).
              05 GFCTOF-CCONTA-EXCEC               PIC  9(013).
              05 GFCTOF-CPACOTE                    PIC  9(005).
              05 GFCTOF-CAGPTO                     PIC  9(003).
              05 GFCTOF-DINI-ADESAO                PIC  X(010).
              05 GFCTOF-SEQUENCIA                  PIC  9(009).
              05 GFCTOF-HINCL                      PIC  X(026).
