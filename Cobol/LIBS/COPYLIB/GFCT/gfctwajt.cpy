      ******************************************************************
      * NOME DA INC - GFCTWAJT                                         *
      * DESCRICAO   - COMUNICACAO WEB X IMS                 - SAIDA    *
      *               FLEXIBILIZACAO PARA AGRUPAMENTO                  *
      *               CONSULTA HISTORICOS                              *
      *               COMBO SEGMENTO FLEXIBILIZACAO VIGENTE - GFCT5173 *
      * TAMANHO     - 1010 - TOTAL                                     *
      *               0350 - FIXO                                      *
      *               0660 - VARIAVEL (33 X 20)                        *
      * DATA        - 27.11.2009                                       *
      * RESPONSAVEL - FRANCISCO LOURENCO - SONDAPROCWORK               *
      ******************************************************************
      *
       01  WAJT-SAIDA.
           05  WAJT-LL                 PIC S9(004) COMP.
           05  WAJT-ZZ                 PIC S9(004) COMP.
           05  WAJT-TRANSACAO          PIC  X(009).
           05  WAJT-FUNCAO             PIC  X(008).
           05  WAJT-DADOS-RST.
               10  WAJT-QTDE-OCOR      PIC  9(007).
               10  WAJT-QTDE-TOT-REG   PIC  9(009).
               10  WAJT-FUNC-BDSCO     PIC  X(007).
               10  WAJT-FILTRO.
                   15  WAJT-FLT-CAGPTO-CTA
                                       PIC  9(003).
               10  WAJT-PONTEIRO.
                   15  WAJT-PTR-CSGMTO-TARIF
                                       PIC  9(003).
           05  WAJT-ERRO               PIC  9(001).
           05  WAJT-COD-MSG-ERRO       PIC  9(007).
           05  WAJT-COD-SQL-ERRO       PIC  9(003).
           05  WAJT-DESC-MSG-ERRO      PIC  X(079).
           05  WAJT-FIM                PIC  X(001).
           05  FILLER                  PIC  X(209).
           05  WAJT-DADOS              OCCURS 20.
               10  WAJT-COD-SEGMTO     PIC  9(003).
               10  WAJT-DESCR-SEGMTO   PIC  X(030).
