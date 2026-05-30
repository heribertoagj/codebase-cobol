      ******************************************************
      *                                                    *
      * NOME BOOK : FRWKGMEN                               *
      * DESCRICAO : ESTRUTURA DE MENSAGENS DA AREA DE      *
      *             MEMORIA DO GAM                         *
      * DATA      : 08/09/2006                             *
      * AUTOR     : CARLA PARALS SENDIN                    *
      * EMPRESA   : CPM/GFT                                *
      * GRUPO     : TI MELHORIAS                           *
      * COMPONENTE: FRAMEWORK ONLINE                       *
      *                                                    *
      ******************************************************
      *                                                    *
      * FRWKGMEN-HEADER.                                   *
      *   FRWKGMEN-COD-LAYOUT        = CODIGO DE LAYOUT    *
      *   FRWKGMEN-TAM-LAYOUT        = TAMANHO DO LAYOUT   *
      * FRWKGMEN-REGISTRO.                                 *
      *   FRWKGMEN-QUANT-MENS     = QUANTIDADE DE MENSAGENS*
      *   FRWKGMEN-TAB-MENS.                               *
      *     FRWKGMEN-REG-MENS.                             *
      *       FRWKGMEN-PARAMETROS = PAR‚METROS DA MENSAGEM *
      *                                                    *
      ******************************************************
      * DATA       AUTOR        MODIFICACAO                *
      * --------   ---------    -------------------------- *
      * DD/MM/AAAA JNNNNNN      XXXXXXXXXXXXXXXXXXXXXXXXXX *
      ******************************************************
       05 FRWKGMEN-HEADER.
          07 FRWKGMEN-COD-LAYOUT PIC X(008) VALUE 'FRWKGMEN'.
          07 FRWKGMEN-TAM-LAYOUT PIC 9(005) VALUE 5095.
       05 FRWKGMEN-REGISTRO.
          07 FRWKGMEN-QUANT-MENS            PIC 9(002).
          07 FRWKGMEN-TAB-MENS.
             09 FRWKGMEN-REG-MENS           OCCURS 10
                     DEPENDING ON FRWKGMEN-QUANT-MENS.
                11 FRWKGMEN-COD-MENS        PIC X(008).
                11 FRWKGMEN-PARAMETROS      PIC X(500).
