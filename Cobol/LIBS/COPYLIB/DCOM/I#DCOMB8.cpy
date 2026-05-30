      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0B8 - DB2PRD.THIST_PCELA_OPER       *
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
       01  B8-ARGUMENTOS-ENTRADA.

       05  B8-COLUNAS-DA-TABELA.
           10  B8-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  B8-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  B8-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  B8-HULT-ATULZ                   PIC X(26).
           10  B8-DMES-VCTO-PTCAO              PIC S9(2)V COMP-3.
           10  B8-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  B8-HSIT-DESC-COML               PIC X(26).
           10  B8-DVCTO-PCELA-DESC             PIC X(10).
           10  B8-VPRINC-PCELA-DESC            PIC S9(15)V9(2) COMP-3.
           10  B8-QTITLO-CHEQ-PCELA            PIC S9(9)V COMP-3.
           10  B8-CBCO-DEPOS                   PIC S9(3)V COMP-3.
           10  B8-CAG-BCRIA-DEPOS              PIC S9(5)V COMP-3.
           10  B8-PTX-JURO-MES                 PIC S9(3)V9(5) COMP-3.
           10  B8-PTX-JURO-ANO                 PIC S9(3)V9(5) COMP-3.
           10  B8-PTX-CUSTO-MES                PIC S9(3)V9(6) COMP-3.
           10  B8-PTX-CUSTO-ANO                PIC S9(3)V9(6) COMP-3.
           10  B8-VCUSTO-PCELA-DESC            PIC S9(15)V9(2) COMP-3.
           10  B8-VJURO-PCELA-DESC             PIC S9(15)V9(2) COMP-3.
           10  B8-VJURO-DIA-PCELA              PIC S9(15)V9(2) COMP-3.
           10  B8-VIOF-PCELA-DESC              PIC S9(15)V9(2) COMP-3.
           10  B8-VPSENT-PCELA-DESC            PIC S9(15)V9(2) COMP-3.
           10  B8-VJURO-APROP-ACUM             PIC S9(15)V9(2) COMP-3.
           10  B8-DULT-APROP-PCELA             PIC X(10).
           10  B8-PALIQT-IOF-PRROG             PIC S9(3)V9(5) COMP-3.
           10  B8-VIOF-PRROG-PCELA             PIC S9(15)V9(2) COMP-3.
           10  B8-PCUSTO-MES-MORA              PIC S9(3)V9(6) COMP-3.
           10  B8-PCUSTO-ANO-MORA              PIC S9(3)V9(6) COMP-3.
           10  B8-PCUSTO-MES-CRATS             PIC S9(3)V9(6) COMP-3.
           10  B8-PCUSTO-ANO-CRATS             PIC S9(3)V9(6) COMP-3.
           10  B8-PALIQT-IOF-VENCD             PIC S9(3)V9(5) COMP-3.
           10  B8-VIOF-PCELA-VENCD             PIC S9(15)V9(2) COMP-3.
           10  B8-VJURO-MORA-GERC              PIC S9(15)V9(2) COMP-3.
           10  B8-VJURO-MORA-LEGAL             PIC S9(15)V9(2) COMP-3.
           10  B8-VSDO-DVDOR-PRINC             PIC S9(15)V9(2) COMP-3.
           10  B8-VCUSTO-PCELA-VENCD           PIC S9(15)V9(2) COMP-3.
           10  B8-VJURO-TRNSF-CRATS            PIC S9(15)V9(2) COMP-3.
           10  B8-VCUSTO-TRNSF-CRATS           PIC S9(15)V9(2) COMP-3.
           10  B8-DTRNSF-CRATS-PCELA           PIC X(10).
           10  B8-DTRNSF-LP-PCELA              PIC X(10).
           10  B8-CREFT-BACEN                  PIC S9(9)V COMP-3.
           10  B8-CCONTR-CORP-PCELA            PIC S9(7)V COMP-3.
           10  B8-DULT-ADTTO-PCELA             PIC X(10).
           10  B8-CINDCD-MOTVO-ADTTO           PIC X(1).
           10  B8-CINDCD-PEND-EXTER            PIC X(1).
           10  B8-CINDCD-BLOQ-CONS             PIC X(1).
           10  B8-DBLOQ-CONS-CCORR             PIC X(10).
           10  B8-DULT-PRROG-PCELA             PIC X(10).
           10  B8-DULT-REATV-PCELA             PIC X(10).
           10  B8-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  B8-CTERM                        PIC X(8).
           10  B8-DINCL-REG                    PIC X(10).
BSI001     10  B8-VJURO-REMUN-VENCD            PIC S9(15)V9(2) COMP-3.
BSI001     10  B8-VMORA-VENCD-DESC             PIC S9(15)V9(2) COMP-3.
BSI001     10  B8-VMULTA-VENCD-DESC            PIC S9(15)V9(2) COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  B8-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  B8-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  B8-RETORNO.
       05  B8-COD-RETORNO                         PIC 9(04).
       05  B8-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  B8-PROXIMO-RESTART                     PIC 9(05).
       05  B8-QTDE-RETORNADA                      PIC 9(03).
       05  B8-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  B8-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TB8.
           10  B8RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  B8RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  B8RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  B8RT-HULT-ATULZ                 PIC X(26).
           10  B8RT-DMES-VCTO-PTCAO            PIC S9(2)V COMP-3.
           10  B8RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  B8RT-HSIT-DESC-COML             PIC X(26).
           10  B8RT-DVCTO-PCELA-DESC           PIC X(10).
           10  B8RT-VPRINC-PCELA-DESC          PIC S9(15)V9(2) COMP-3.
           10  B8RT-QTITLO-CHEQ-PCELA          PIC S9(9)V COMP-3.
           10  B8RT-CBCO-DEPOS                 PIC S9(3)V COMP-3.
           10  B8RT-CAG-BCRIA-DEPOS            PIC S9(5)V COMP-3.
           10  B8RT-PTX-JURO-MES               PIC S9(3)V9(5) COMP-3.
           10  B8RT-PTX-JURO-ANO               PIC S9(3)V9(5) COMP-3.
           10  B8RT-PTX-CUSTO-MES              PIC S9(3)V9(6) COMP-3.
           10  B8RT-PTX-CUSTO-ANO              PIC S9(3)V9(6) COMP-3.
           10  B8RT-VCUSTO-PCELA-DESC          PIC S9(15)V9(2) COMP-3.
           10  B8RT-VJURO-PCELA-DESC           PIC S9(15)V9(2) COMP-3.
           10  B8RT-VJURO-DIA-PCELA            PIC S9(15)V9(2) COMP-3.
           10  B8RT-VIOF-PCELA-DESC            PIC S9(15)V9(2) COMP-3.
           10  B8RT-VPSENT-PCELA-DESC          PIC S9(15)V9(2) COMP-3.
           10  B8RT-VJURO-APROP-ACUM           PIC S9(15)V9(2) COMP-3.
           10  B8RT-DULT-APROP-PCELA           PIC X(10).
           10  B8RT-PALIQT-IOF-PRROG           PIC S9(3)V9(5) COMP-3.
           10  B8RT-VIOF-PRROG-PCELA           PIC S9(15)V9(2) COMP-3.
           10  B8RT-PCUSTO-MES-MORA            PIC S9(3)V9(6) COMP-3.
           10  B8RT-PCUSTO-ANO-MORA            PIC S9(3)V9(6) COMP-3.
           10  B8RT-PCUSTO-MES-CRATS           PIC S9(3)V9(6) COMP-3.
           10  B8RT-PCUSTO-ANO-CRATS           PIC S9(3)V9(6) COMP-3.
           10  B8RT-PALIQT-IOF-VENCD           PIC S9(3)V9(5) COMP-3.
           10  B8RT-VIOF-PCELA-VENCD           PIC S9(15)V9(2) COMP-3.
           10  B8RT-VJURO-MORA-GERC            PIC S9(15)V9(2) COMP-3.
           10  B8RT-VJURO-MORA-LEGAL           PIC S9(15)V9(2) COMP-3.
           10  B8RT-VSDO-DVDOR-PRINC           PIC S9(15)V9(2) COMP-3.
           10  B8RT-VCUSTO-PCELA-VENCD         PIC S9(15)V9(2) COMP-3.
           10  B8RT-VJURO-TRNSF-CRATS          PIC S9(15)V9(2) COMP-3.
           10  B8RT-VCUSTO-TRNSF-CRATS         PIC S9(15)V9(2) COMP-3.
           10  B8RT-DTRNSF-CRATS-PCELA         PIC X(10).
           10  B8RT-DTRNSF-LP-PCELA            PIC X(10).
           10  B8RT-CREFT-BACEN                PIC S9(9)V COMP-3.
           10  B8RT-CCONTR-CORP-PCELA          PIC S9(7)V COMP-3.
           10  B8RT-DULT-ADTTO-PCELA           PIC X(10).
           10  B8RT-CINDCD-MOTVO-ADTTO         PIC X(1).
           10  B8RT-CINDCD-PEND-EXTER          PIC X(1).
           10  B8RT-CINDCD-BLOQ-CONS           PIC X(1).
           10  B8RT-DBLOQ-CONS-CCORR           PIC X(10).
           10  B8RT-DULT-PRROG-PCELA           PIC X(10).
           10  B8RT-DULT-REATV-PCELA           PIC X(10).
           10  B8RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  B8RT-CTERM                      PIC X(8).
           10  B8RT-DINCL-REG                  PIC X(10).
BSI001     10  B8RT-VJURO-REMUN-VENCD          PIC S9(15)V9(2) COMP-3.
BSI001     10  B8RT-VMORA-VENCD-DESC           PIC S9(15)V9(2) COMP-3.
BSI001     10  B8RT-VMULTA-VENCD-DESC          PIC S9(15)V9(2) COMP-3.
