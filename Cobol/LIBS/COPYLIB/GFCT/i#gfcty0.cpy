      *****************************************************************
      * NOME DA INC - I#GFCTY0                                        *
      * DESCRICAO   - LIBERACAO DE SUSPENSAO DE TARIFA PARA AGPTO     *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 150                                             *
      * DATA        - 01/2007                                         *
      * RESPONSAVEL - EVANISE    - PROCWORK - 4152-43532  RAMAL 211   *
      *****************************************************************
       01  GFCTY0-ENTRADA.
           03 GFCTY0-LL                            PIC S9(004) COMP.
           03 GFCTY0-ZZ                            PIC S9(004) COMP.
           03 GFCTY0-TRANSACAO                     PIC  X(009).
           03 GFCTY0-FUNCAO                        PIC  X(008).
           03 GFCTY0-DADOS-RST.
              05 GFCTY0-FILTRO.
                 07 GFCTY0-AGPTO                   PIC  9(003).
                 07 GFCTY0-TARIFA                  PIC  9(005).
              05 GFCTY0-PONTEIRO.
                 07 GFCTY0-PONT-CSEQ-AGPTO         PIC  9(009).
                 07 GFCTY0-PONT-TARIFA             PIC  9(005).
                 07 GFCTY0-PONT-AGENCIA            PIC  9(005).
                 07 GFCTY0-PONT-POSTO              PIC  9(005).
                 07 GFCTY0-PONT-CNPJ-CPF           PIC  9(009).
                 07 GFCTY0-PONT-FILIAL             PIC  9(005).
                 07 GFCTY0-PONT-CONTROLE           PIC  9(002).
                 07 GFCTY0-PONT-COD-MUNIC          PIC  9(007).
                 07 GFCTY0-PONT-UF                 PIC  X(002).
                 07 GFCTY0-PONT-DATA-SUSP          PIC  X(010).
              05 GFCTY0-QTDE-OCOR                  PIC  9(003).
              05 GFCTY0-QTDE-TOT-REG               PIC  9(006).
              05 GFCTY0-FUNC-BDSCO                 PIC  X(007).
              05 GFCTY0-FIM                        PIC  X(001).
              05 FILLER                            PIC  X(045).
