      ******************************************************************
      * NOME DA INC - GFCTWADB - GFCT5546                              *
      * DESCRICAO   - TARIFAS - EXTRATO ANUAL DE TARIFAS               *
      *               COMBO DE DATA/HR DE SOLIC PREV EMISS CORREIO EXTR*
      *               COMUNICACAO WEB X IMS - SAIDA                    *
      * TAMANHO     - 1010 (GERAL)                                     *
      *             - 0560 (PARTE FIXA)                                *
      *             - 0450 (PARTE VARIAVEL)                            *
      *             - 0015 (CADA OCORRENCIA)                           *
      * DATA        - 10/2008                                          *
      * RESPONSAVEL - VALERIA TORQUATO - SONDA PROCWORK - 4506-2349    *
      ******************************************************************
      *
       01  GFCTWADB-SAIDA.
           03  GFCTWADB-LL                       PIC S9(004) COMP.
           03  GFCTWADB-ZZ                       PIC S9(004) COMP.
           03  GFCTWADB-TRANSACAO                PIC  X(009).
           03  GFCTWADB-FUNCAO                   PIC  X(008).
           03  GFCTWADB-QTDE-OCOR                PIC  9(003).
           03  GFCTWADB-QTDE-TOT-REG             PIC  9(006).
           03  GFCTWADB-FUNC-BDSCO               PIC  X(007).
           03  GFCTWADB-FILTRO.
               05  GFCTWADB-FTR-ANO              PIC  9(004).
               05  GFCTWADB-FTR-ACAO             PIC  X(001).
      *    'H' - CONSULTA     = HISTORICO
      *    'V' - CANCELAMENTO = VIGENTE
           03  GFCTWADB-PONTEIRO.
               05  GFCTWADB-PTR-TIMEST           PIC  X(026).
           03  GFCTWADB-ERRO                     PIC  9(001).
           03  GFCTWADB-COD-MSG-ERRO             PIC  9(007).
           03  GFCTWADB-COD-SQL-ERRO             PIC  9(003).
           03  GFCTWADB-DESC-MSG-ERRO            PIC  X(079).
           03  GFCTWADB-FIM                      PIC  X(001).
           03  FILLER                            PIC  X(236).
           03  GFCTWADB-DADOS                    OCCURS 15.
               05  GFCTWADB-DATA-INCLUSAO        PIC  X(010).
               05  GFCTWADB-HORA-INCLUSAO        PIC  X(005).
               05  GFCTWADB-TIMEST-SOLIC         PIC  X(026).
