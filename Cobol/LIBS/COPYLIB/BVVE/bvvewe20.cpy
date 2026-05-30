      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEWE20                     *
      *            COMUNICACAO COM A CIP - MENSAGEM RRC0020            *
      *----------------------------------------------------------------*
      *
            05     FILLER      REDEFINES BVVEWE00-MENSAGEM.
      *            IDENTIFICADOR DA OPERAÁ„O PARA CANCELAMENTO.
             07    BVVEWE20-IDENTD-OP                       PIC X(019).
             07    BVVEWE20-MAPA.
      *            FIXO 'CANCELAMENTO' (MINUSCULO)
              10   BVVEWE20-OP                              PIC X(012).
      *            IDENTIFICADOR NEGOCIAÁ„O RECEBÌVEL
              10   BVVEWE20-IDENTDNEGCRECBVL                PIC X(040).
      *            INDICADOR CANCELAMENTO DE VALOR TOTAL
              10   BVVEWE20-INDRCANCELVLRTOTAL              PIC X(001).
      *            INDICADOR LIQUIDAÁ„O OPERAÁ„O
              10   BVVEWE20-INDRLIQUIDOP                    PIC X(001).
      *            INDICADOR CANCELAMENTO DE CESS„O A CONSTITUIR
              10   BVVEWE20-INDRCANCELCESCONSTITR           PIC X(001).
