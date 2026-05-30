      ******************************************************************
      * COPYBOOK  : DCOMWOPS                                           *
      * DESCRICAO : BOOK DE SAIDA - LISTA DE PARCELAS DA OPERACAO      *
      * DATA      : MAIO/2021                                          *
      * AUTOR     : GABRIEL SALIM                                      *
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
      * DCOMWOPS-BLOCO-SAIDA                                           *
      * DCOMWOPS-TIPO-PRAZO-TOT = TIPO DO PRAZO TOTAL, FIXO "MES"      *
      * DCOMWOPS-PRAZO-TOT      = PRAZO TOTAL DA OPERACAO EM MESES     *
      * DCOMWOPS-TIPO-PRAZO-REST= TIPO DE PRAZO RESTANTE, FIXO "MES"   *
      * DCOMWOPS-PRAZO-REST     = PRAZO RESTANTE DA OPERACAO EM MESES  *
      * DCOMWOPS-QTD-PARC-PAG   = QUANTIDADE DE PARCELAS PAGAS         *
      * DCOMWOPS-QTD-PARC-AVCR  = QUANTIDADE DE APRCELAS A VENCER      *
      * DCOMWOPS-QTD-PARC-VENC  = QUANTIDADE DE PARCELAS VENCIDAS      *
      * DCOMWOPS-MOEDA          = MOEDA UTILIDAZA                      *
      * DCOMWOPS-QTDE-PARCELA   = QUANTIDADE DE PARCELAS               *
      *     LISTA                                                      *
      * DCOMWOPS-DATA-VENC      = DATA DO VENCIMENTO DA PARCELA        *
      * DCOMWOPS-VALOR-PARC     = VALOR DA PARCELA                     *
      * DCOMWOPS-NUM-PARC       = NUMERO DA PARCELA NO SISTEMA DCOM    *
      ******************************************************************
          05 DCOMWOPS-HEADER.
             10 DCOMWOPS-COD-LAYOUT        PIC X(008) VALUE 'DCOMWOPS'.
             10 DCOMWOPS-TAM-LAYOUT        PIC 9(005) VALUE 3559.
             10 DCOMWOPS-VERSAO            PIC X(005).
          05 DCOMWOPS-BLOCO-SAIDA.
             10 DCOMWOPS-TIPO-PRAZO-TOT    PIC  X(003) VALUE 'MES'.
             10 DCOMWOPS-PRAZO-TOT         PIC  9(006).
             10 DCOMWOPS-TIPO-PRAZO-REST   PIC  X(003) VALUE 'MES'.
             10 DCOMWOPS-PRAZO-REST        PIC  9(006).
             10 DCOMWOPS-QTD-PARC-PAG      PIC  9(005).
             10 DCOMWOPS-QTD-PARC-AVCR     PIC  9(005).
             10 DCOMWOPS-QTD-PARC-VENC     PIC  9(005).
             10 DCOMWOPS-QTDE-PARCELA      PIC  9(005).
             10 DCOMWOPS-QTDE-OCORR        PIC  9(003).
             10 DCOMWOPS-OCORRENCIAS    OCCURS  100 TIMES.
                15 DCOMWOPS-DATA-VENC        PIC  X(010).
                15 DCOMWOPS-MOEDA            PIC  X(003) VALUE 'BRL'.
                15 DCOMWOPS-VALOR-PARC       PIC  9(15)V9(2).
                15 DCOMWOPS-NUM-PARC         PIC  9(005).
