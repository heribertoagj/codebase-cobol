      *****************************************************************
      * NOME DA INC - I#GFCTNZ                                        *
      * DESCRICAO   - EXCECAO ADESAO COMPULSORIA - CONSULTA           *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTNZ-LL   - 1010                                            *
      * DATA        - ABRIL/2006                                      *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTNZ-ENTRADA.
           03 GFCTNZ-LL                            PIC S9(004) COMP.
           03 GFCTNZ-ZZ                            PIC S9(004) COMP.
           03 GFCTNZ-TRANSACAO                     PIC  X(009).
           03 GFCTNZ-FUNCAO                        PIC  X(008).
           03 GFCTNZ-DADOS-RST.
              05 GFCTNZ-QTDE-OCOR                  PIC  9(003).
              05 GFCTNZ-QTDE-TOT-REG               PIC  9(006).
              05 GFCTNZ-FUNC-BDSCO                 PIC  X(007).
              05 GFCTNZ-FILTRO.
                 07 GFCTNZ-FILTRO-PACOTE           PIC  9(005).
                 07 GFCTNZ-FILTRO-AGPTO            PIC  9(003).
                 07 GFCTNZ-FILTRO-AGENCIA          PIC  9(005).
                 07 GFCTNZ-FILTRO-CONTA            PIC  9(013).
                 07 GFCTNZ-FILTRO-SEQ              PIC  9(009).
              05 GFCTNZ-PONTEIRO.
                 07 GFCTNZ-PONT-PACOTE             PIC  9(005).
                 07 GFCTNZ-PONT-AGPTO              PIC  9(003).
                 07 GFCTNZ-PONT-AGENCIA            PIC  9(005).
                 07 GFCTNZ-PONT-CONTA              PIC  9(013).
                 07 GFCTNZ-PONT-SEQ                PIC  9(009).
                 07 GFCTNZ-PONT-DINI               PIC  X(010).
                 07 GFCTNZ-PONT-HINI               PIC  X(026).
              05 GFCTNZ-FIM                        PIC  X(001).
           03 FILLER                               PIC  X(875).
