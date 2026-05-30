      *================================================================*
      * DESCRICAO  : LIBERACAO DA OPERACAO                             *
      * COPYBOOK   : DCOMWAQE - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1AQI - GERAR CONTRATO ARQUITETURA             *
      * FLUXO      : DCOMIABO                                          *
      * DATA       : 14/12/2024                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - CAPGEMINI                    *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *================================================================*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWAQE-FUNCAO       = INDICA A FUNCIONALIDADE:               *
      *-> OBRIGATORIO         - 1 - LIBERACAO - INCLUIR PROTOCOLO      *
      *->                       2 - LIBERACAO - CONSULTAR PROTOCOLO    *
      *================================================================*
      *
          05 DCOMWAQE-E-HEADER.
             10 DCOMWAQE-COD-LAYOUT     PIC  X(008) VALUE 'DCOMWAQE'.
             10 DCOMWAQE-TAM-LAYOUT     PIC  9(005) VALUE 00100.
      *
          05 DCOMWAQE-BLOCO-ENTRADA.
             10 DCOMWAQE-FUNCAO         PIC  9(001).
             10 DCOMWAQE-DANO-OPER-DESC PIC  9(004).
             10 DCOMWAQE-NSEQ-OPER-DESC PIC  9(009).
             10 DCOMWAQE-CTPO-LIBRC     PIC  X(001).
             10 DCOMWAQE-VLIBRC         PIC  9(015)V9(002).
      *
      *-->   AREA RESERVADA PARA EXPANSAO
      *-->   =========================================
             10 DCOMWAQE-FILLER         PIC  X(055).
      *
