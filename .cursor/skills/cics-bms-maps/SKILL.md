---
name: cics-bms-maps
description: Desenvolvimento CICS transacional COBOL com mapas BMS (SEND/RECEIVE MAP, RESP, HANDLE CONDITION) no padrao Bradesco. Use para telas CICS, mapas BMS, transacao online, SEND MAP, RECEIVE MAP ou arquivos .mfs em demandas AGROLEG.
---

# CICS Transacional e Mapas BMS (Bradesco)

Orienta programas **CICS** em COBOL com **BMS** para telas transacionais, integrado ao padrao desta workspace.

## Integracao com outras skills

- Estrutura COBOL (SECTION, `-99-FIM`, indentacao): `bradesco-style`
- Quality gate e erros: `bradesco-quality-code`
- Pipeline de demanda: `demanda-workflow`
- Mapas em demanda: registrar em README (`Escopo tecnico` / `Fontes impactados`)

## Estrutura BMS

| Macro | Nivel | Descricao |
|---|---|---|
| DFHMSD | Mapset | Conjunto de mapas |
| DFHMDI | Mapa | Cada mapa no mapset |
| DFHMDF | Campo | Campos do mapa |

## Copybook gerado pelo BMS

- Sufixo **I** (Input): dados do terminal
- Sufixo **O** (Output): dados para a tela
- Incluir copybook com nome padrao do member BMS; nao alterar estrutura gerada

## Comandos principais

### SEND MAP

```cobol
EXEC CICS SEND MAP('NOMEMAPA')
              MAPSET('NOMEMAPSET')
              [ERASE]
              [FREEKB]
              [FRSET]
     END-EXEC.
```

### RECEIVE MAP com RESP

```cobol
EXEC CICS RECEIVE MAP('NOMEMAPA')
                 MAPSET('NOMEMAPSET')
                 RESP(WRK-CICS-RESP)
     END-EXEC.
IF WRK-CICS-RESP NOT = DFHRESP(NORMAL) AND
   WRK-CICS-RESP NOT = DFHRESP(MAPFAIL)
    ... tratamento de erro ...
END-IF.
```

**MAPFAIL**: enter sem alteracao — pode ser esperado (ex.: saida).

### HANDLE CONDITION

Codificar **antes** do comando protegido. Preferir SECTION dedicada com `-99-FIM. EXIT.` (`bradesco-style`).

```cobol
EXEC CICS HANDLE CONDITION
    MAPFAIL(1200-TRATAR-MAPFAIL)
    ERROR(9999-ROTINA-ERRO)
END-EXEC.
```

## Padrao Bradesco nesta workspace

1. Logica CICS dentro de **SECTION** numerada com fechamento `-99-FIM. EXIT.`
2. Usar `RESP()` em SEND MAP, RECEIVE MAP, LINK e comandos criticos
3. Rotina de erro: padrao `9999-ROTINA-ERRO` / `POOL7100` quando aplicavel ao programa
4. Inicializar campos **O** antes de SEND MAP em tela nova; usar **FRSET** para reset de atributos
5. Nao colocar logica de negocio extensa dentro de paragrafo de HANDLE — `PERFORM` section de tratamento
6. Ordenar DFHMDF por **POS** numerica para performance

## Boas praticas

| Pratica | Objetivo |
|---|---|
| Separar apresentacao e negocio | Manutencao e teste |
| Tratar MAPFAIL explicitamente | Evitar abend em enter em branco |
| Campos unicos no mapset | Evitar conflito CICS |
| Mensagem clara no rodape | Ex.: "NAO HA MAIS REGISTROS" em EOF de paginacao |

## Referencia historica

- AGROLEG-993: RUEC7000 + RUECT700 (Tipo Exploracao, retorno tela imovel)
- AGROLEG-993: RUCA6690 (PF8 sem registros — mensagem rodape)

## Checklist CICS

- [ ] Copybook BMS incluido e alinhado ao `.mfs`
- [ ] RESP() testado em comandos criticos
- [ ] MAPFAIL tratado quando aplicavel
- [ ] SECTION com `-99-FIM. EXIT.`
- [ ] Campos de saida inicializados antes de SEND MAP
