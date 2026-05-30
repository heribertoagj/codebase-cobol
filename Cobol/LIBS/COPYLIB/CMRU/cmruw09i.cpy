      ******************************************************************
      * DESCRICAO.: AREA DE COMUNICACAO COM FUNCIONAL                  *
      * DATA......: 09/2016                                            *
      * AUTOR.....: BSI TECNOLOGIA - ALPHA                             *
      * EMPRESA...: BSI TECNOLOGIA                                     *
      ******************************************************************
      * DATA        AUTOR             DESCRICAO / MANUTENCAO           *
      ******************************************************************
      * 08/09/2016  BSI               LISTA TIPOS DE BENEFICIARIO      *
      *                               ( BOOK DO SERVICO CMRU309C )     *
      ******************************************************************
      *  ENTRADA                                                       *
      *  E-CTPO-BNEFC-RURAL : CODIGO DO TIPO DE BENEFICIARIO (INTERNO) *
      *                                                                *
      *  SAIDA                                                         *
      *  S-CTPO-BNEFC-RURAL : CODIGO DO TIPO DE BENEFICIARIO (INTERNO) *
      *  S-CTPO-BNEFC-BACEN : CODIGO DO TIPO DE BENEFIICARIO (BACEN)   *
      *  S-ITPO-BNEFC-RURAL : DESCRICAO DO TIPO DE BENEFICIARIO        *
      *  S-DINIC-VGCIA-REG  : DATA DE INICIO DE VIGENCIA DO PARAMETRO  *
      *  S-DFIM-VGCIA-REG   : DATA DE FIM DE VIGENCIA DO PARAMETRO     *
      *  S-CSIT-REG-RURAL   : SITUACAO DO PARAMETRO (A - ATIVO)        *
      *                                             (I - INATIVO)      *
      ******************************************************************
      *
       05 CMRUW09I-HEADER.
         10 CMRUW09I-COD-LAYOUT            PIC  X(008) VALUE 'CMRUW09I'.
         10 CMRUW09I-TAM-LAYOUT            PIC  9(005) VALUE 00230.
       05 CMRUW09I-REGISTRO.
         10 CMRUW09I-BLOCO-RETORNO.
           15 CMRUW09I-COD-RETORNO             PIC  9(002).
           15 CMRUW09I-COD-ERRO                PIC  X(004).
           15 CMRUW09I-MENSAGEM                PIC  X(079).
         10 CMRUW09I-BLOCO-ENTRADA.
           15 CMRUW09I-E-CTPO-BNEFC-RURAL      PIC  9(002).
         10 CMRUW09I-BLOCO-SAIDA.
           15 CMRUW09I-S-CTPO-BNEFC-RURAL      PIC  9(002).
           15 CMRUW09I-S-CTPO-BNEFC-BACEN      PIC  X(003).
           15 CMRUW09I-S-ITPO-BNEFC-RURAL      PIC  X(060).
           15 CMRUW09I-S-DINIC-VGCIA-REG       PIC  X(010).
           15 CMRUW09I-S-DFIM-VGCIA-REG        PIC  X(010).
           15 CMRUW09I-S-CSIT-REG-RURAL        PIC  X(001).
           15 CMRUW09I-S-CUSUAR-INCL           PIC  X(009).
           15 CMRUW09I-S-HINCL-REG             PIC  X(026).
           15 CMRUW09I-S-CUSUAR-MANUT          PIC  X(009).
           15 CMRUW09I-S-HMANUT-REG            PIC  X(026).
       05 CMRUW09I-S-ERRO-AREA.
         10 CMRUW09I-S-ERR-TIPO-ACESSO         PIC  X(03).
         10 CMRUW09I-S-ERR-PGM                 PIC  X(08).
         10 CMRUW09I-S-ERR-DADOS-IMS-DB2.
           15 CMRUW09I-S-ERR-DBD-TAB           PIC  X(18).
           15 CMRUW09I-S-ERR-FUN-COMANDO       PIC  X(10).
           15 CMRUW09I-S-ERR-STA-CODE          PIC  X(04).
           15 CMRUW09I-S-ERR-SQL-CODE  REDEFINES
              CMRUW09I-S-ERR-STA-CODE          PIC S9(09) COMP-4.
           15 CMRUW09I-S-ERR-LOCAL             PIC  X(04).
           15 CMRUW09I-S-ERR-SEGM              PIC  X(08).
           15 CMRUW09I-S-FILLER                PIC  X(31).
       05 CMRUW09I-S-SQLCA                     PIC  X(136).
