      ******************************************************************
      * NOME BOOK : PSDCWBSI - INTERFACE DO COORDENADOR E FUNCIONAL    *
      * DESCRICAO :                                                    *
      * DATA      : 03/03/2010                                         *
      * EMPRESA   : CPM BRAXIS                                         *
      * GRUPO     : CPM BRAXIS                                         *
      * COMPONENTE:                                                    *
      * TAMANHO   : 31 BYTES                                           *
      *********************** DADOS DE RETORNO *************************
      *                                                                *
      * USUARIO-ALF: CODIGO DO USUARIO ALFANUMERICO                    *
      * USUARIO-NUM: CODIGO DO USUARIO CONVERTIDO PARA NUMERICO        *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 PSDCWBSI-HEADER.
             10 PSDCWBSI-COD-LAYOUT    PIC X(08)   VALUE 'PSDCWBSI'.
             10 PSDCWBSI-TAM-LAYOUT    PIC 9(05)   VALUE 31.
           05 PSDCWBSI-REGISTRO.
             10 PSDCWBSI-BLOCO-ENTRADA.
               15 PSDCWBSI-E-USUARIO-ALF           PIC X(09).
             10 PSDCWBSI-BLOCO-SAIDA.
               15 PSDCWBSI-S-USUARIO-NUM           PIC 9(09).

