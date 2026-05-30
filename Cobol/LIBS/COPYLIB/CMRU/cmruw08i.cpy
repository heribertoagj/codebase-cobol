      ******************************************************************
      * DESCRICAO.: AREA DE COMUNICACAO COM FUNCIONAL                  *
      * DATA......: 09/2016                                            *
      * AUTOR.....: BSI TECNOLOGIA - ALPHA                             *
      * EMPRESA...: BSI TECNOLOGIA                                     *
      ******************************************************************
      * DATA        AUTOR             DESCRICAO / MANUTENCAO           *
      ******************************************************************
      * 08/09/2016  BSI               LISTA TIPOS DE BENEFICIARIO      *
      *                               ( BOOK DO SERVICO CMRU308L )     *
      ******************************************************************
      *  RETORNO                                                       *
      *  COD-RETORNO        : 00 - CONSULTA EFETUADA, SEM MAIS         *
      *                            OCORRENCIAS                         *
      *                     : 04 - CONSULTA EFETUADA, FIM DAS          *
      *                            OCORRENCIAS                         *
      *                     : 08 - CONSULTA NAO EFETUADA, ARGUMENTO    *
      *                            INVALIDO                            *
      *                     : 12 - CONSULTA NAO EFETUADA, ERRO DB2     *
      *                                                                *
      *  ENTRADA                                                       *
      *  E-MAX-OCORR        : QUANTIDADE DE OCORRENCIAS (MAX. 50)      *
      *                                                                *
      *  E-QTDE-DESPR       : QUANTIDADE DE OCORRENCIAS A DESPREZAR    *
      *                                                                *
      *  E-DINIC-VGCIA-REG  : INICIO DO PERIODO PARA FILTRO COM '<='   *
      *                       FORMATO 'DD.MM.AAAA'.                    *
      *                       SE SPACES, SERVICO UTILIZA DATA CORRENTE *
      *                                                                *
      *  E-DFIM-VGCIA-REG   : INICIO DO PERIODO PARA FILTRO COM '>='   *
      *                       FORMATO 'DD.MM.AAAA'.                    *
      *                       SE SPACES, SERVICO UTILIZA DATA CORRENTE *
      *                                                                *
      *  E-CSIT-REG-RURAL   : SITUACAO DO PARAMETRO (A - ATIVO)        *
      *                                             (I - INATIVO)      *
      *                       SE SPACES, SERVICO UTILIZA 'A'           *
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
       05 CMRUW08I-HEADER.
         10 CMRUW08I-COD-LAYOUT            PIC  X(008) VALUE 'CMRUW08I'.
         10 CMRUW08I-TAM-LAYOUT            PIC  9(005) VALUE 5448.
       05 CMRUW08I-REGISTRO.
         10 CMRUW08I-BLOCO-RETORNO.
           15 CMRUW08I-COD-RETORNO             PIC  9(002).
           15 CMRUW08I-COD-ERRO                PIC  X(004).
           15 CMRUW08I-MENSAGEM                PIC  X(079).
         10 CMRUW08I-BLOCO-ENTRADA.
           15 CMRUW08I-E-MAX-OCORR             PIC  9(005).
           15 CMRUW08I-E-QTDE-DESPR            PIC  9(005).
           15 CMRUW08I-E-CTPO-BNEFC-RURAL      PIC  9(002).
           15 CMRUW08I-E-ITPO-BNEFC-RURAL      PIC  X(040).
           15 CMRUW08I-E-DINIC-VGCIA-REG       PIC  X(010).
           15 CMRUW08I-E-DFIM-VGCIA-REG        PIC  X(010).
           15 CMRUW08I-E-CSIT-REG-RURAL        PIC  X(001).
         10 CMRUW08I-BLOCO-SAIDA.
           15 CMRUW08I-S-QTDE-OCORR            PIC  9(005).
           15 CMRUW08I-S-LISTA-DADOS           OCCURS 50 TIMES.
             20 CMRUW08I-S-CTPO-BNEFC-RURAL    PIC  9(002).
             20 CMRUW08I-S-CTPO-BNEFC-BACEN    PIC  X(003).
             20 CMRUW08I-S-ITPO-BNEFC-RURAL    PIC  X(040).
             20 CMRUW08I-S-DINIC-VGCIA-REG     PIC  X(010).
             20 CMRUW08I-S-DFIM-VGCIA-REG      PIC  X(010).
             20 CMRUW08I-S-CSIT-REG-RURAL      PIC  X(001).
             20 CMRUW08I-S-CUSUAR-MANUT        PIC  X(009).
             20 CMRUW08I-S-HMANUT-REG          PIC  X(026).
       05 CMRUW08I-S-ERRO-AREA.
         10 CMRUW08I-S-ERR-TIPO-ACESSO         PIC  X(03).
         10 CMRUW08I-S-ERR-PGM                 PIC  X(08).
         10 CMRUW08I-S-ERR-DADOS-IMS-DB2.
           15 CMRUW08I-S-ERR-DBD-TAB           PIC  X(18).
           15 CMRUW08I-S-ERR-FUN-COMANDO       PIC  X(10).
           15 CMRUW08I-S-ERR-STA-CODE          PIC  X(04).
           15 CMRUW08I-S-ERR-SQL-CODE  REDEFINES
              CMRUW08I-S-ERR-STA-CODE          PIC S9(09) COMP-4.
           15 CMRUW08I-S-ERR-LOCAL             PIC  X(04).
           15 CMRUW08I-S-ERR-SEGM              PIC  X(08).
           15 CMRUW08I-S-FILLER                PIC  X(31).
       05 CMRUW08I-S-SQLCA                     PIC  X(136).
