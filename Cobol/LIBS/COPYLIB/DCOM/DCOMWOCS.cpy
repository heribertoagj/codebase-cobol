      ******************************************************************
      * COPYBOOK  : DCOMWOCS                                           *
      * DESCRICAO : BOOK DE SAIDA - DETALHE  CONTRATOS DCOM            *
      * DATA      : 12/05/2021                                         *
      * AUTOR     : LUIS EDUARDO                                       *
      * EMPRESA   : BRQ                                                *
      * GRUPO     : OPEN BANKING - VS FORNECIMENTO                     *
      * COMPONENTE: DCOM - DESCONTO COMERCIAL                          *
      *----------------------------------------------------------------*
      * ALTERACOES:                                                    *
      *                                                                *
      *    DATA    NOME       DESCRICAO                                *
      * XX/XX/XXXX XXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                                *
      *----------------------------------------------------------------*
      * DCOMWOCS-BLOCO-SAIDA                                           *
      * DCOMWOCS-S-OPERACAO         = NUMERO DA OPERACAO               *
      * DCOMWOCS-S-WID-PDRAO-OPER   = CODIGO DA OPERACAO               *
      * DCOMWOCS-S-DEXPARA-MOD      = NOME DO PRODUTO                  *
      * DCOMWOCS-S-CMODLD-RZ-CTBIL  = TIPO + SUB-TIPO DO PRODUTO       *
      * DCOMWOCS-S-DT-OPERACAO      = DATA DA OPERACAO                 *
      * DCOMWOCS-S-DT-INIC-OPER     = DATA INICIO DA OPERACAO          *
      * DCOMWOCS-S-DT-QUITACAO      = DATA QUITACAO DA OPERACAO        *
      * DCOMWOCS-S-VL-OPERACAO      = VALOR DA OPERACAO                *
      * DCOMWOCS-S-TP-MOEDA         = TIPO MOEDA                       *
      *                               "BRL" - REAL                     *
      * DCOMWOCS-S-DT-FNAL-OPER     = DATA FINAL DA OPERACAO           *
      * DCOMWOCS-S-PERIODICIDADE    = PERIODICIDADE DAS PARCELAS       *
      *                               "SEM_PERIODICIDADE_REGULAR"      *
      * DCOMWOCS-S-DT-VCTO-INICIAL  = DATA 1O VENCIMENTO               *
      * DCOMWOCS-S-VL-CET           = VALOR CET                        *
      * DCOMWOCS-S-TP-AMORTIZACAO   = TIPO AMORTIZACAO                 *
      *                               "SEM_SISTEMA_AMORTIZACAO"        *
      * DCOMWOCS-S-TP-TAXA          = TIPO TAXA                        *
      *                               "EFETIVA"                        *
      * DCOMWOCS-S-TP-RATE          = TIPO TARIFA DE JUROS             *
      *                               "SIMPLES" / "COMPOSTO"           *
      * DCOMWOCS-S-RATE-FREQ        = FREQUENCIA DO JUROS              *
      *                               AM - MENSAL                      *
      *                               AA - ANUAL                       *
      * DCOMWOCS-S-TP-CALC          = TIPO CALCULO JUROS               *
      *                               1 - UTEIS    (21/25)             *
      *                               2 - CORRIDOS (30/360)            *
      *                               3 - CORRIDOS (30/365)            *
      * DCOMWOCS-S-TP-INDEX         = TIPO INDICE DE JUROS             *
      *                               "PRE_FIXADO"                     *
      * DCOMWOCS-S-STP-INDEX        = SUB TIPO INDICE DE JUROS         *
      *                               "PRE_FIXADO"                     *
      * DCOMWOCS-S-INDEX-INFO       = INFO ADICIONAL INDICE            *
      * DCOMWOCS-S-VL-PRE-RATE      = VALOR PRE INDICE DE JUROS        *
      * DCOMWOCS-S-VL-POS-RATE      = VALOR POS INDICE DE JUROS        *
      * DCOMWOCS-S-INFO             = INFORMACAO ADICIONAL             *
      * DCOMWOCS-S-NM-TARIFA        = NOME DA TARIFA                   *
      *                               "TARIFA DA OPERACAO"             *
      * DCOMWOCS-S-CD-TARIFA        = CODIGO DA TARIFA                 *
      *                               "TAC"                            *
      * DCOMWOCS-S-TP-TARIFA        = TIPO DA TARIFA                   *
      *                               "UNICA"                          *
      * DCOMWOCS-S-REGRA-TARIFA     = REGRA DA TARIFA                  *
      *                               "MINIMO"                         *
      *                               "MAXIMO"                         *
      *                               "FIXO"                           *
      *                               "PERCENTUAL"                     *
      * DCOMWOCS-S-VL-TARIFA        = VALOR DA TARIFA                  *
      * DCOMWOCS-S-VL-TARIFA-PERC   = PERCENTUAL DA TARIFA             *
      * DCOMWOCS-S-QTDE-ENCARGOS    = QUANTIDADE OCORRENCIA            *
      * LISTA DE ENCARGOS DO CONTRATO :                                *
      * DCOMWOCS-S-TP-ENCARGO       = NOME DO ENCARGO                  *
      *                               "JUROS_REMUNERATORIOS_POR_ATRASO"*
      *                               "MULTA_ATRASO_PAGAMENTO"         *
      *                               "JUROS_MORA_ATRASO"              *
      *                               "IOF_CONTRATACAO"                *
      *                               "IOF_POR_ATRASO"                 *
      * DCOMWOCS-S-INFO-ADICIONAL   = INFORMACAO ADICIONAL             *
      * DCOMWOCS-S-VL-ENGARGO       = VALOR DO ENCARGO                 *
      ******************************************************************
          05 DCOMWOCS-HEADER.
             10 DCOMWOCS-COD-LAYOUT        PIC X(008) VALUE 'DCOMWOCS'.
             10 DCOMWOCS-TAM-LAYOUT        PIC 9(005) VALUE 13841.
             10 DCOMWOCS-VERSAO            PIC X(005).
          05 DCOMWOCS-BLOCO-SAIDA.
             10 DCOMWOCS-S-OPERACAO              PIC  9(013).
             10 DCOMWOCS-S-WID-PDRAO-OPER        PIC  X(067).
             10 DCOMWOCS-S-DEXPARA-MOD           PIC  X(036).
             10 DCOMWOCS-S-CMODLD-RZ-CTBIL       PIC  9(004).
             10 DCOMWOCS-S-DT-OPERACAO           PIC  X(010).
             10 DCOMWOCS-S-DT-INIC-OPER          PIC  X(010).
             10 DCOMWOCS-S-DT-QUITACAO           PIC  X(010).
             10 DCOMWOCS-S-VL-OPERACAO           PIC  9(015)V9(2).
             10 DCOMWOCS-S-TP-MOEDA              PIC  X(003).
             10 DCOMWOCS-S-DT-FNAL-OPER          PIC  X(010).
             10 DCOMWOCS-S-PERIODICIDADE         PIC  X(025).
             10 DCOMWOCS-S-DT-VCTO-INICIAL       PIC  X(010).
             10 DCOMWOCS-S-VL-CET                PIC  9(008)V9(7).
             10 DCOMWOCS-S-TP-AMORTIZACAO        PIC  X(023).
             10 DCOMWOCS-S-TP-TAXA               PIC  X(007).
             10 DCOMWOCS-S-TP-RATE               PIC  X(008).
             10 DCOMWOCS-S-RATE-FREQ             PIC  X(002).
             10 DCOMWOCS-S-TP-CALC               PIC  9(001).
             10 DCOMWOCS-S-TP-INDEX              PIC  X(010).
             10 DCOMWOCS-S-STP-INDEX             PIC  X(010).
             10 DCOMWOCS-S-INDEX-INFO            PIC  X(005).
             10 DCOMWOCS-S-VL-PRE-RATE           PIC  9(008)V9(7).
             10 DCOMWOCS-S-VL-POS-RATE           PIC  9(008)V9(7).
             10 DCOMWOCS-S-INFO                  PIC  X(005).
             10 DCOMWOCS-S-NM-TARIFA             PIC  X(018).
             10 DCOMWOCS-S-CD-TARIFA             PIC  X(003).
             10 DCOMWOCS-S-TP-TARIFA             PIC  X(005).
             10 DCOMWOCS-S-REGRA-TARIFA          PIC  X(010).
             10 DCOMWOCS-S-VL-TARIFA             PIC  9(015)V9(2).
             10 DCOMWOCS-S-VL-TARIFA-PERC        PIC  9(008)V9(7).
             10 DCOMWOCS-S-QTDE-ENCARGOS         PIC  9(003).
             10 DCOMWOCS-S-OCOR-ENCARGOS   OCCURS 10 TIMES.
                15 DCOMWOCS-S-TP-ENCARGO         PIC  X(035).
                15 DCOMWOCS-S-INFO-ADICIONAL     PIC  X(001).
                15 DCOMWOCS-S-VL-ENGARGO         PIC  9(008)V9(7).
