      ******************************************************************
      * NOME BOOK   : CMTFWS0E                                         *
      * DESCRICAO   : AREA DE CONTROLE DE ERRO NOS LAYOUTS DE MIGRACAO *
      *               GOTF.                                            *
      * DATA        : ABRIL / 2012                                     *
      * EMPRESA     : SONDA PROCWORK                                   *
      * AUTOR       : SONDA PROCWORK                                   *
      * GRUPO       : CMTF  - CONVIVENCIA E MIGRACAO TARIFAS           *
      * TAMANHO     : 00620 BYTES                                      *
      ******************************************************************
      *   DATA          AUTOR             DESCRICAO / MANUTENCAO       *
      * DD/MM/AA     XXXXXXXXXXX     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                              XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      ******************************************************************
           05 CMTFWS0E-REG.
              07 CMTFWS0E-AREA-ERRO.
                 09 CMTFWS0E-TAB-ERRO           OCCURS 10 TIMES.
                    10 CMTFWS0E-COD-ERRO        PIC  9(02).
                    10 CMTFWS0E-CAMPO-ERRO      PIC  X(20).
                    10 CMTFWS0E-MENSAGEM-ERRO   PIC  X(40).
