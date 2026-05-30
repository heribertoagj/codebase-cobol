***************************** Top of Data ******************************
      *****************************************************************
      * NOME DA INC - I#GFCTKM                                        *
      * DESCRICAO   - COMBO DE UF                                     *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 09.02.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL.: 4152-4352    *
      *****************************************************************
      *
       01  GFCTKM-SAIDA.
           03  GFCTKM-LL                            PIC S9(004) COMP.
           03  GFCTKM-ZZ                            PIC S9(004) COMP.
           03  GFCTKM-TRANSACAO                     PIC  X(009).
           03  GFCTKM-FUNCAO                        PIC  X(008).
           03  GFCTKM-FILTRO.
               05  GFCTKM-FILTRO-UF                 PIC  X(002).
           03  GFCTKM-QTDE-OCOR                     PIC  9(003).
           03  GFCTKM-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTKM-FUNC-BDSCO                    PIC  X(007).
           03  GFCTKM-PONTEIRO.
               05  GFCTKM-PONTEIRO-UF               PIC  X(002).
           03  GFCTKM-ERRO                          PIC  9(001).
           03  GFCTKM-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTKM-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTKM-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTKM-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(014).
           03  GFCTKM-DADOS.
               05  FILLER                           OCCURS 27.
                   07  GFCTKM-COD-UF                PIC  X(002).
                   07  GFCTKM-DESC-UF               PIC  X(030).
