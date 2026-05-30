      ******************************************************************
      * COPYBOOK  : DCOMWOVS                                           *
      * DESCRICAO : BOOK DE SAIDA - LISTA DE PAGAMENTOS DAS PARCELAS   *
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
      * DCOMWOVS-BLOCO-SAIDA                                           *
      * DCOMWOVS-PARC-PAGAS       = TOTAL PARCELAS PAGAS               *
      * DCOMWOVS-SALDO-PEND-CONTR = SALDO TOTAL A PAGAR                *
      * DCOMWOVS-QTD-PAG          = QTD OCORRENCIAS - PARCELAS PAGAS   *
      *# LISTA PARCELAS PAGAS                                          *
      * DCOMWOVS-ID-PAG           = IDENTIFICA TIPO PAGAMENTO          *
      * DCOMWOVS-IDENT-PAG        = PAGAMENTO PACTUADO / AVULSO        *
      * DCOMWOVS-ID-PARC          = NUMERO DA PARCELA PAGA             *
      * DCOMWOVS-DT-PAG           = DATA PAGAMENTO                     *
      * DCOMWOVS-MOEDA-PAG        = TIPO MOEDA                         *
      * DCOMWOVS-VLR-PAGO         = VALOR PAGAMENTO                    *
      * DCOMWOVS-QTD-TARIF        = QUANTIDADE DE TARIFAS PAGAS        *
      *# LISTA TARIFAS                                                 *
      * DCOMWOVS-NOME-TARIF       = NOME TARIFA                        *
      * DCOMWOVS-COD-TARIF        = CODIGO TARIFA                      *
      * DCOMWOVS-VLR-TARIF        = VALOR TARIFA                       *
      *# FIM LISTA TARIFAS                                             *
      * DCOMWOVS-QTD-ENCAR        = QUANTIDADE ENCARGOS PAGOS          *
      *# LISTA ENCARGOS                                                *
      * DCOMWOVS-TIPO-ENCAR       = TIPO ENCARGO                       *
      * DCOMWOVS-INF-ADC-ENCAR    = INFORMACAO ADICIONAIS              *
      * DCOMWOVS-VLR-ENCAR        = VALOR DO ENCARGO                   *
      *# FIM LISTA ENCARGOS                                            *
      *# FIM LISTA PARCELAS PAGAS                                      *
      ******************************************************************
          05 DCOMWOVS-HEADER.
             10 DCOMWOVS-COD-LAYOUT        PIC X(008) VALUE 'DCOMWOVS'.
             10 DCOMWOVS-TAM-LAYOUT        PIC 9(005) VALUE 14965.
             10 DCOMWOVS-VERSAO            PIC X(005).
          05 DCOMWOVS-BLOCO-SAIDA.
             10 DCOMWOVS-PARC-PAGAS             PIC  9(005).
             10 DCOMWOVS-SALDO-PEND-CONTR       PIC  9(15)V9(4).
             10 DCOMWOVS-QTD-PAG                PIC  9(003).
             10 DCOMWOVS-OCORRENCIAS-PAG     OCCURS  020 TIMES.
                15 DCOMWOVS-ID-PAG              PIC  X(030).
                15 DCOMWOVS-IDENT-PAG           PIC  X(005).
                15 DCOMWOVS-ID-PARC             PIC  X(010).
                15 DCOMWOVS-DT-PAG              PIC  X(010).
                15 DCOMWOVS-MOEDA-PAG           PIC  X(003) VALUE 'BRL'.
                15 DCOMWOVS-VLR-PAGO            PIC  9(015)V9(4).
                15 DCOMWOVS-QTD-TARIF           PIC  9(003).
                15 DCOMWOVS-OCORRENCIAS-TARIF.
                   20 DCOMWOVS-NOME-TARIF       PIC  X(010).
                   20 DCOMWOVS-COD-TARIF        PIC  X(010).
                   20 DCOMWOVS-VLR-TARIF        PIC  9(015)V9(4).
                15 DCOMWOVS-QTD-ENCAR           PIC  9(003).
                15 DCOMWOVS-OCORRENCIAS-ENCAR   OCCURS  006 TIMES.
                   20 DCOMWOVS-TIPO-ENCAR       PIC  X(035).
                   20 DCOMWOVS-INF-ADC-ENCAR    PIC  X(050).
                   20 DCOMWOVS-VLR-ENCAR        PIC  9(015)V9(4).
