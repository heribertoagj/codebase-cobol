      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0G5 - DB2PRD.TTRASI_PCELA_OPER      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
BSI001*                    ALTERACAO - BSI TECNOLOGIA                 *
BSI001*---------------------------------------------------------------*
BSI001*  ANALISTA BSI:   FERNANDO LUIZ DE SANTI  -  BSI TECNOLOGIA    *
BSI001*  DATA........:   28/04/2017                                   *
BSI001*---------------------------------------------------------------*
BSI001*  OBJETIVO....:   INCLUIR CAMPOS DE ENCARGOS MORATORIOS.       *
BSI001*****************************************************************
       01  G5-ARGUMENTOS-ENTRADA.

       05  G5-COLUNAS-DA-TABELA.
           10  G5-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  G5-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  G5-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  G5-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  G5-HSIT-DESC-COML               PIC X(26).
           10  G5-DVCTO-PCELA-DESC             PIC X(10).
           10  G5-VPRINC-PCELA-DESC            PIC S9(15)V9(2) COMP-3.
           10  G5-QTITLO-CHEQ-PCELA            PIC S9(9)V COMP-3.
********** 10  G5-CBCO-DEPOS                   PIC S9(3)V COMP-3.
********** 10  G5-CAG-BCRIA-DEPOS              PIC S9(5)V COMP-3.
           10  G5-PTX-JURO-MES                 PIC S9(3)V9(5) COMP-3.
           10  G5-PTX-JURO-ANO                 PIC S9(3)V9(5) COMP-3.
           10  G5-PTX-CUSTO-MES                PIC S9(3)V9(6) COMP-3.
           10  G5-PTX-CUSTO-ANO                PIC S9(3)V9(6) COMP-3.
           10  G5-VCUSTO-PCELA-DESC            PIC S9(15)V9(2) COMP-3.
           10  G5-VJURO-PCELA-DESC             PIC S9(15)V9(2) COMP-3.
           10  G5-VJURO-DIA-PCELA              PIC S9(15)V9(2) COMP-3.
           10  G5-VIOF-PCELA-DESC              PIC S9(15)V9(2) COMP-3.
           10  G5-VPSENT-PCELA-DESC            PIC S9(15)V9(2) COMP-3.
           10  G5-VJURO-APROP-ACUM             PIC S9(15)V9(2) COMP-3.
           10  G5-DULT-APROP-PCELA             PIC X(10).
           10  G5-PALIQT-IOF-PRROG             PIC S9(3)V9(5) COMP-3.
           10  G5-VIOF-PRROG-PCELA             PIC S9(15)V9(2) COMP-3.
           10  G5-PCUSTO-MES-MORA              PIC S9(3)V9(6) COMP-3.
           10  G5-PCUSTO-ANO-MORA              PIC S9(3)V9(6) COMP-3.
           10  G5-PCUSTO-MES-CRATS             PIC S9(3)V9(6) COMP-3.
           10  G5-PCUSTO-ANO-CRATS             PIC S9(3)V9(6) COMP-3.
           10  G5-PALIQT-IOF-VENCD             PIC S9(3)V9(5) COMP-3.
           10  G5-VIOF-PCELA-VENCD             PIC S9(15)V9(2) COMP-3.
           10  G5-VJURO-MORA-GERC              PIC S9(15)V9(2) COMP-3.
           10  G5-VJURO-MORA-LEGAL             PIC S9(15)V9(2) COMP-3.
           10  G5-VSDO-DVDOR-PRINC             PIC S9(15)V9(2) COMP-3.
           10  G5-VCUSTO-PCELA-VENCD           PIC S9(15)V9(2) COMP-3.
           10  G5-VJURO-TRNSF-CRATS            PIC S9(15)V9(2) COMP-3.
           10  G5-VCUSTO-TRNSF-CRATS           PIC S9(15)V9(2) COMP-3.
           10  G5-DTRNSF-CRATS-PCELA           PIC X(10).
           10  G5-DTRNSF-LP-PCELA              PIC X(10).
           10  G5-CBACEN-DESC-RURAL            PIC S9(9)V COMP-3.
           10  G5-CCONTR-CORP-PCELA            PIC S9(7)V COMP-3.
           10  G5-DULT-ADTTO-PCELA             PIC X(10).
           10  G5-CINDCD-MOTVO-ADTTO           PIC X(1).
           10  G5-CINDCD-PEND-EXTER            PIC X(1).
           10  G5-CINDCD-BLOQ-CONS             PIC X(1).
           10  G5-DBLOQ-CONS-CCORR             PIC X(10).
           10  G5-DULT-PRROG-PCELA             PIC X(10).
           10  G5-DULT-REATV-PCELA             PIC X(10).
           10  G5-HULT-ATULZ                   PIC X(26).
           10  G5-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  G5-CTERM                        PIC X(8).
           10  G5-DINCL-REG                    PIC X(10).
BSI001     10  G5-VJURO-REMUN-VENCD            PIC S9(15)V9(2) COMP-3.
BSI001     10  G5-VMORA-VENCD-DESC             PIC S9(15)V9(2) COMP-3.
BSI001     10  G5-VMULTA-VENCD-DESC            PIC S9(15)V9(2) COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  G5-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  G5-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  G5-RETORNO.
       05  G5-COD-RETORNO                         PIC 9(04).
       05  G5-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  G5-PROXIMO-RESTART                     PIC 9(05).
       05  G5-QTDE-RETORNADA                      PIC 9(03).
       05  G5-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  G5-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TG5.
           10  G5RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  G5RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  G5RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  G5RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  G5RT-HSIT-DESC-COML             PIC X(26).
           10  G5RT-DVCTO-PCELA-DESC           PIC X(10).
           10  G5RT-VPRINC-PCELA-DESC          PIC S9(15)V9(2) COMP-3.
           10  G5RT-QTITLO-CHEQ-PCELA          PIC S9(9)V COMP-3.
********** 10  G5RT-CBCO-DEPOS                 PIC S9(3)V COMP-3.
********** 10  G5RT-CAG-BCRIA-DEPOS            PIC S9(5)V COMP-3.
           10  G5RT-PTX-JURO-MES               PIC S9(3)V9(5) COMP-3.
           10  G5RT-PTX-JURO-ANO               PIC S9(3)V9(5) COMP-3.
           10  G5RT-PTX-CUSTO-MES              PIC S9(3)V9(6) COMP-3.
           10  G5RT-PTX-CUSTO-ANO              PIC S9(3)V9(6) COMP-3.
           10  G5RT-VCUSTO-PCELA-DESC          PIC S9(15)V9(2) COMP-3.
           10  G5RT-VJURO-PCELA-DESC           PIC S9(15)V9(2) COMP-3.
           10  G5RT-VJURO-DIA-PCELA            PIC S9(15)V9(2) COMP-3.
           10  G5RT-VIOF-PCELA-DESC            PIC S9(15)V9(2) COMP-3.
           10  G5RT-VPSENT-PCELA-DESC          PIC S9(15)V9(2) COMP-3.
           10  G5RT-VJURO-APROP-ACUM           PIC S9(15)V9(2) COMP-3.
           10  G5RT-DULT-APROP-PCELA           PIC X(10).
           10  G5RT-PALIQT-IOF-PRROG           PIC S9(3)V9(5) COMP-3.
           10  G5RT-VIOF-PRROG-PCELA           PIC S9(15)V9(2) COMP-3.
           10  G5RT-PCUSTO-MES-MORA            PIC S9(3)V9(6) COMP-3.
           10  G5RT-PCUSTO-ANO-MORA            PIC S9(3)V9(6) COMP-3.
           10  G5RT-PCUSTO-MES-CRATS           PIC S9(3)V9(6) COMP-3.
           10  G5RT-PCUSTO-ANO-CRATS           PIC S9(3)V9(6) COMP-3.
           10  G5RT-PALIQT-IOF-VENCD           PIC S9(3)V9(5) COMP-3.
           10  G5RT-VIOF-PCELA-VENCD           PIC S9(15)V9(2) COMP-3.
           10  G5RT-VJURO-MORA-GERC            PIC S9(15)V9(2) COMP-3.
           10  G5RT-VJURO-MORA-LEGAL           PIC S9(15)V9(2) COMP-3.
           10  G5RT-VSDO-DVDOR-PRINC           PIC S9(15)V9(2) COMP-3.
           10  G5RT-VCUSTO-PCELA-VENCD         PIC S9(15)V9(2) COMP-3.
           10  G5RT-VJURO-TRNSF-CRATS          PIC S9(15)V9(2) COMP-3.
           10  G5RT-VCUSTO-TRNSF-CRATS         PIC S9(15)V9(2) COMP-3.
           10  G5RT-DTRNSF-CRATS-PCELA         PIC X(10).
           10  G5RT-DTRNSF-LP-PCELA            PIC X(10).
           10  G5RT-CBACEN-DESC-RURAL          PIC S9(9)V COMP-3.
           10  G5RT-CCONTR-CORP-PCELA          PIC S9(7)V COMP-3.
           10  G5RT-DULT-ADTTO-PCELA           PIC X(10).
           10  G5RT-CINDCD-MOTVO-ADTTO         PIC X(1).
           10  G5RT-CINDCD-PEND-EXTER          PIC X(1).
           10  G5RT-CINDCD-BLOQ-CONS           PIC X(1).
           10  G5RT-DBLOQ-CONS-CCORR           PIC X(10).
           10  G5RT-DULT-PRROG-PCELA           PIC X(10).
           10  G5RT-DULT-REATV-PCELA           PIC X(10).
           10  G5RT-HULT-ATULZ                 PIC X(26).
           10  G5RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  G5RT-CTERM                      PIC X(8).
           10  G5RT-DINCL-REG                  PIC X(10).
BSI001     10  G5RT-VJURO-REMUN-VENCD          PIC S9(15)V9(2) COMP-3.
BSI001     10  G5RT-VMORA-VENCD-DESC           PIC S9(15)V9(2) COMP-3.
BSI001     10  G5RT-VMULTA-VENCD-DESC          PIC S9(15)V9(2) COMP-3.
