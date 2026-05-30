      *****************************************************************
      * NOME DA INC - I#GFCTO4                                        *
      * DESCRICAO   - COMBO TIPO AGRUPAMENTO - INCLUS O -  SAIDA      *
      * TAMANHO     - 1010                                            *
      * GFCTO4-LL   - 1010                                            *
      * DATA        - ABRIL/2006                                      *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      * CONTEMPLAR  TODOS AGRUP. FIXOS MENOS EMPRESA.                 *
      *****************************************************************
       01  GFCTO4-SAIDA.
           03 GFCTO4-LL                            PIC S9(004) COMP.
           03 GFCTO4-ZZ                            PIC S9(004) COMP.
           03 GFCTO4-TRANSACAO                     PIC  X(009).
           03 GFCTO4-FUNCAO                        PIC  X(008).
           03 GFCTO4-DADOS-REST.
              05 GFCTO4-QTDE-OCOR                  PIC  9(003).
              05 GFCTO4-QTDE-TOT-REG               PIC  9(006).
              05 GFCTO4-FUNC-BDSCO                 PIC  X(007).
              05 GFCTO4-TERMINAL                   PIC  X(008).
              05 GFCTO4-FILTRO.
                 07 GFCTO4-FILTRO-AGENCIA          PIC  9(005).
                 07 GFCTO4-FILTRO-CONTA            PIC  9(013).
                 07 GFCTO4-FILTRO-PACOTE           PIC  9(005).
              05 GFCTO4-PONTEIRO.
                 07 GFCTO4-PONT-AGENCIA            PIC  9(005).
                 07 GFCTO4-PONT-CONTA              PIC  9(013).
                 07 GFCTO4-PONT-PACOTE             PIC  9(005).
                 07 GFCTO4-PONT-CAGPTO             PIC  9(003).
              05 GFCTO4-FIM                        PIC  X(001).
           03 GFCTO4-ERRO                          PIC  9(001).
           03 GFCTO4-COD-MSG-ERRO                  PIC  9(007).
           03 GFCTO4-COD-SQL-ERRO                  PIC  9(003).
           03 GFCTO4-DESC-MSG-ERRO                 PIC  X(079).
           03 GFCTO4-FILLER                        PIC  X(009).
           03 GFCTO4-OCORRENCIAS                   OCCURS 06 TIMES.
              05 GFCTO4-CAGPTO                     PIC  9(003).
              05 GFCTO4-RAGPTO                     PIC  X(030).
              05 GFCTO4-SEQUENCIA                  PIC  9(009).
              05 GFCTO4-DINI-ADESAO                PIC  X(010).
              05 GFCTO4-CHAVE-NUM9                 PIC  9(009).
              05 GFCTO4-CHAVE-NUM5                 PIC  9(005).
ST2504        05 GFCTO4-CHAVE-CCGC-CPF             PIC  X(009).
ST2504        05 GFCTO4-CHAVE-FILIAL               PIC  X(004).
              05 GFCTO4-CHAVE-NUM2                 PIC  9(002).
              05 GFCTO4-CHAVE-ALP2                 PIC  X(002).
              05 GFCTO4-INDIC-TOTAL                PIC  X(001).
              05 GFCTO4-DATA-INICIO                PIC  X(010).
              05 GFCTO4-DATA-FIM                   PIC  X(010).
              05 GFCTO4-DESC-ARGUMENTO             PIC  X(025).
              05 GFCTO4-DESC-AUX                   PIC  X(020).
