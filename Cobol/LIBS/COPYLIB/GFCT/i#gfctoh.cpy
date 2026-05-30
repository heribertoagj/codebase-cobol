      *****************************************************************
      * NOME DA INC - I#GFCTOH                                        *
      * DESCRICAO   - EXCECAO ADESAO COMPULSORIA -COMBO ARGUMENTOS    *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTOH-LL   - 1010                                            *
      * DATA        - ABRIL/2006                                      *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTOH-ENTRADA.
           03 GFCTOH-LL                            PIC S9(004) COMP.
           03 GFCTOH-ZZ                            PIC S9(004) COMP.
           03 GFCTOH-TRANSACAO                     PIC  X(009).
           03 GFCTOH-FUNCAO                        PIC  X(008).
           03 GFCTOH-DADOS-RST.
              05 GFCTOH-QTDE-OCOR                  PIC  9(003).
              05 GFCTOH-QTDE-TOT-REG               PIC  9(006).
              05 GFCTOH-FUNC-BDSCO                 PIC  X(007).
              05 GFCTOH-FILTRO.
                 07 GFCTOH-FILTRO-PACOTE           PIC  9(005).
                 07 GFCTOH-FILTRO-AGPTO            PIC  9(003).
              05 GFCTOH-PONTEIRO.
                 07 GFCTOH-PONT-PACOTE             PIC  9(005).
                 07 GFCTOH-PONT-AGPTO              PIC  9(003).
                 07 GFCTOH-PONT-SEQ                PIC  9(009).
              05 GFCTOH-FIM                        PIC  X(001).
              05 FILLER                            PIC  X(947).
